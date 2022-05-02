include "util.inc"
include "bios13.inc"
include "gpt.inc"
include "ext2.inc"

use16
org 7c00h

;globals
virtual at 0x600
  partition	GPTPartition
  inode 	Ext2_INode
  block_ext     dd 52 dup ?
end virtual
virtual at 0x8000
  superblock	Ext2_Superblock
end virtual

init:
        xor	ax, ax
        mov	ss, ax
        mov	sp, 0x7C00
        push	es
        push	di
        mov	es, ax
        cld
        lea	di, [partition]
        mov	cx, GPTPartition.sizeof / 2
        rep movsw
        mov	ds, ax
        jmp     start



print:	mov	ah, 0xE
        mov	bx, 0x7
@@:	lodsb
        test	al, al
        jz	@f
        cmp	al, 0xA
        int	0x10
        jne	@b
        mov	al, 0xD
        int	0x10
        jmp	@b
@@:	ret



exit_print:
        lea	si, [str_error]
        call	print
        pop	si
        call	print
        jmp	$



str_error:
        string	"ERROR: "



; arguments:
;   EAX - block number
;   EDX - diskNum
;   start.logBlockSectSize - log block size in sectors
;   start.dap - preinitialized disk address packet
;   partition - GPT partition struct
; destroys:
;   EAX = ?, CX = [start.logBlockSectSize], EBX = ?, [start.dap.startSector] = ?
readBlock:
        mov	cx, [start.logBlockSectSize]
        xor	ebx, ebx
        shld	ebx, eax, cl
        shl	eax, cl
        add	eax, dword [partition.firstLBA]
        adc	ebx, dword [partition.firstLBA + 4]
        mov	dword[start.dap.startSector], eax
        mov	dword[start.dap.startSector + 4], ebx
        push    esi
        push    edi
        lea	si, [start.dap]
        mov	ah, BIOS.DiskService.ExtendedRead
        int	BIOS.DiskService
        jnc	@f
        call	exit_print
str_error_read:
        strline "Reading"
@@:	pop     edi
        pop     esi
        ret



start:
virtual at bp
        dw	?
  .pnp	far_ptr
end virtual

begin_stack
  .driveParams\
        DriveParameters
  .dap	DiskAddrPacket
  .logBlockSectSize\
        dw	?
        dalign	2, .stack_start
end_stack

        enter	.stack_size, 0
        lea	di, [.stack_start]
        mov	cl, .stack_size / 2
        rep stosw
        mov	byte[.driveParams.size], DriveParameters.sizeof
        lea	si, [.driveParams]
        mov	ah, BIOS.DiskService.ExtDriveParams
        int	BIOS.DiskService
        jnc     .params_request_success
        call    exit_print
        strline "Parameters"
.params_request_success:
        mov     byte [.dap.size], DiskAddrPacket.sizeof
        mov     [.dap.dstPtr.segment], 0x7C0
        lea     di, [.dap.startSector]
        lea     si, [partition.firstLBA]
        mov     cl, 4
        rep movsw
        bsf     cx, [.driveParams.sectorSize]
        jnz     .sector_size_non_zero
        call    exit_print
        strline "Zero sector size"
.sector_size_non_zero:
        bsr     bx, [.driveParams.sectorSize]
        cmp     bx, cx
        jz      .sector_size_is_pot
        call    exit_print
        strline "Non POT sector size"
.sector_size_is_pot:
        cmp     cx, 11
        jge     .loadNotNeed
        mov     [.dap.sectorCount], 2048
        shr     [.dap.sectorCount], cl
        lea     si, [.dap]
        mov     ah, BIOS.DiskService.ExtendedRead
        int     BIOS.DiskService
        jnc     .second_half_load_success
        call    exit_print
        strline "Second half"

        db      510 - ($ - $$) dup 0
        dw      0xAA55

.second_half_load_success:
.loadNotNeed:
        cmp     [superblock.magic], Ext2_Superblock_Magic
        jz      .is_superblock
        call    exit_print
.str_error_superblock:
        strline "Invalid superblock"

.is_superblock:
        neg     cx
        add     cx, word[superblock.log_block_size]
        add     cx, 10

        jns     .sector_size_lequal_block_size
        call	exit_print
.str_error_small_block_size:
        strline "Block less than sector"

.sector_size_lequal_block_size:
        mov	[.dap.sectorCount], 1
        shl	[.dap.sectorCount], cl

        mov	[.logBlockSectSize], cx

        mov	[.dap.dstPtr.segment], 0x1000

.read_boot_inode:
        mov     eax, Ext2_Bootloader_INode - 1
        push    dx
        xor     edx, edx
        div     [superblock.inodes_per_group]
        assume  eax, iNodeGroup
        assume  edx, inGrpINodeIdx

        mov     cl, Ext2_LogMinBlockSize - Ext2_LogBGDSize
        add     cl, byte[superblock.log_block_size]
        mov     edi, iNodeGroup
        assume  edi, iNodeGroup
        assume  eax, blkWithINodeGrp
        shr     blkWithINodeGrp, cl

        mov     esi, blkWithINodeGrp
        shl     esi, cl
        sub     iNodeGroup, esi
        restore iNodeGroup
        assume  di, locINodeGrpIdx

        add     blkWithINodeGrp, [superblock.first_data_block]
        inc     blkWithINodeGrp
        mov     esi, inGrpINodeIdx
        assume  esi, inGrpINodeIdx
        pop     dx
        call    readBlock
        restore blkWithINodeGrp

        mov     cl, Ext2_LogMinBlockSize
        add     cl, byte[superblock.log_block_size]
        bsr     ax, [superblock.inode_size]
        sub     cl, al
        assume  eax, blkWithINode
        mov     blkWithINode, inGrpINodeIdx
        shr     blkWithINode, cl
        mov     ebx, blkWithINode
        shl     ebx, cl
        sub     inGrpINodeIdx, ebx
        restore inGrpINodeIdx
        assume  si, inBlkINodeIdx

        shl     locINodeGrpIdx, Ext2_LogBGDSize
        push    ds
        lds     bx, [.dap.dstPtr]
        mov     edi, [bx + locINodeGrpIdx + Ext2_BlockGroupDescriptor.inode_table]
        restore locINodeGrpIdx
        pop     ds
        add     blkWithINode, edi
        call    readBlock
        restore blkWithINode

        bsr     cx, [superblock.inode_size]
        shl     inBlkINodeIdx, cl
        push    ds
        lds     bx, [.dap.dstPtr]
        lea     di, [inode]
        mov     cx, Ext2_INode.sizeof / 2
        rep movsw
        pop     ds
        restore inBlkINodeIdx

.read_inode_data:
        mov     cl, Ext2_LogMinBlockSize
        add     cl, byte[superblock.log_block_size]
        assume  esi, maxblks
        xor     maxblks, maxblks
        inc     si
        shl     maxblks, cl
        push    maxblks
        dec     maxblks
        add     maxblks, [inode.size]
        shr     maxblks, cl
        mov     eax, 0x10000
        shr     eax, cl
        cmp     eax, maxblks
        cmovc   maxblks, eax
        assume  si, maxblks

        cmp     maxblks, 12
        jng     @f
        mov     eax, [inode.blockIndir]
        call    readBlock
        push    si
        push    ds
        lds     si, [.dap.dstPtr]
        lea     di, [block_ext]
        mov     cx, 52 * 4 / 2
        rep movsw
        pop     ds
        pop     si

@@:     xor     cx, cx
        assume  cx, i
        pop     edi
        shr     edi, 4
@@:     cmp     i, maxblks
        jnl     @f
        mov     bx, i
        shl     bx, 2
        cmp     i, 12
        jnl     .i_not_less_12
        assume  eax, block
        mov     block, [inode.block + bx]
        jmp     .block_set
.i_not_less_12:
        sub     bx, 12 * 4
        mov     block, [block_ext + bx]
.block_set:
        push    cx
        call    readBlock
        pop     cx
        add     [.dap.dstPtr.segment], di
        inc     i
        jmp     @b
        restore maxblks
        restore i
        restore block
@@:     leave
        pop     di
        pop     es
        lea     si, [partition]
        jmp far 0x1000:0
