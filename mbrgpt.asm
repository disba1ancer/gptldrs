include "util.inc"
include "bios13.inc"
include "gpt.inc"

use16
org 0x7C00

; init data segments and copy loader to 0x600
	xor	eax, eax
	mov	ds, ax
	mov	ss, ax
	mov	sp, 0x7C00
	mov	si, sp
	push	es
	push	di
	mov	es, ax
	cld
	mov	di, 0x600
	mov	cx, 0x200 / 2
	rep movsw
	jmp	far 0:start

org 0x600 + $ - $$

virtual at 0x800
gptEntryCache GPTPartition
gptEntry GPTPartition
gptHeader GPTHeader
end virtual



start:
diskNum equ	dl
virtual at bp - .stackSize
.stackStart:

.dap	DiskAddrPacket
.drvInf DriveParameters

.stackEnd:
.stackSize = $ - .stackStart
.oldBP	dw	?
.pnp	far_ptr
end virtual
	enter	.stackSize, 0
	lea	di, [.stackStart]
	mov	cl, .stackSize / 2
	rep stosw
	; load drive parameters
	mov	[.drvInf.size], DriveParameters.sizeof
	mov	ah, BIOS.DiskService.ExtDriveParams
	lea	si, [.drvInf]
	int	BIOS.DiskService
        jnc	.get_info_no_error
        call	.exit_print
.error_info:
        strline "Drive parameters"
.get_info_no_error:
	; prepare address packet
	inc	ax
	mov	byte [.dap.size], DiskAddrPacket.sizeof
	mov	[.dap.sectorCount], ax
	mov	byte [.dap.dstPtr.offset + 1], 0x10
	mov	word [.dap.startSector], ax
	; read GPT header
	mov	ah, BIOS.DiskService.ExtendedRead
	lea	si, [.dap]
	int	BIOS.DiskService
        jnc	.read_header_no_error
        call    .exit_print
.error_read:
        strline "Reading"
.read_header_no_error:
	mov	si, [.dap.dstPtr.offset]
	lea	di, [gptHeader]
	mov	cl, GPTHeader.sizeof / 2
	rep movsw
	lea	si, [gptHeader.partitionArrayStart]
	lea	di, [.dap.startSector]
	mov	cl, 4
	rep movsw
	mov	si, [.dap.dstPtr.offset]
	add	si, [.drvInf.sectorSize]
	mov	bx, si
	mov	ecx, [gptHeader.partitionEntries]
	mov	byte [gptEntry.attributes], al
.search_part_loop:
		push	cx
		lea	di, [gptEntryCache]
		mov	cx, GPTPartition.sizeof
	.copy_loop:
			cmp	bx, si
			jne	.load_not_need
                        mov	ah, BIOS.DiskService.ExtendedRead
                        lea	si, [.dap]
                        int	BIOS.DiskService
                        jnc	.read_no_error
                        push    .error_read
			jmp	.exit_print
		.read_no_error:
			call	inc_sector_num
		.load_not_need:
			movsb
			loop	.copy_loop
		mov	cx, -GPTPartition.sizeof
		add	cx, word [gptHeader.partitionEntrySize]
	.seek_loop:
			sub	si, bx
			add	cx, si
			jnc	.exit_seek_loop
			jz	.exit_seek_loop
			call	inc_sector_num
			jmp	.seek_loop
	.exit_seek_loop:
		mov	si, bx
		add	si, cx
		test	byte [gptEntryCache.attributes], 4
		jz	.part_is_inactive
		mov	cx, 8
		lea	di, [gptEntryCache.typeGUID]
		repe scasw
		jz	.part_is_not_used
		test	byte [gptEntry.attributes], 4
                jz	.no_active_part_found_yet
                push	.error_found_second_active_part
                jmp	.exit_print
	.no_active_part_found_yet:
		push	si
		lea	di, [gptEntry]
		lea	si, [gptEntryCache]
		mov	cl, GPTPartition.sizeof / 2
		rep movsw
		pop	si
	.part_is_inactive:
	.part_is_not_used:
		pop	cx
		loopd	.search_part_loop
	test	byte [gptEntry.attributes], 4
        jnz	.active_part_found
        call	.exit_print
.error_found_second_active_part:
.error_active_part_not_found:
        strline "Partition table"
.active_part_found:
	lea	di, [.dap.startSector]
	lea	si, [gptEntry.firstLBA]
	mov	cl, 4
	rep movsw
	mov	[.dap.dstPtr.offset], 0x7c00
	mov	ah, BIOS.DiskService.ExtendedRead
	lea	si, [.dap]
	int	BIOS.DiskService
	cmp	word [0x7dfe], 0xaa55
        jz	.loader_found
        call	.exit_print
.error_boootloader_not_found:
        strline "Bootloader"
.loader_found:
	lea	si, [gptEntry]
	;leave
	;pop	 di
	;pop	 es
        les	di, [.pnp]
	jmp	far 0:0x7c00
.exit_print:
	mov	si, error_str
	call	print
	pop	si
	call	print
	jmp	$



inc_sector_num:
	mov	si, [start.dap.dstPtr.offset]
        xor	eax, eax
	stc
	adc	dword [start.dap.startSector], eax
	adc	dword [start.dap.startSector + 4], eax
	ret



print:	mov	bx, 0x0007	 ; prints zero terminated string pointed by SI
	mov	ah, 0xE 	 ; destroys ax, bx, bp
.loop:	lodsb
	test	al, al
	jz	.exit
	cmp	al, 10
	int	0x10
	jne	.loop
	add	al, 3
	int	0x10
	jmp	.loop
.exit:	ret



error_str:
        string	"ERROR: "
