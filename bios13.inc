struct DiskAddrPacket
{
  .size 	db	?
  .reserved	db	?
  .sectorCount	dw	?
  .dstPtr	far_ptr
  .startSector	dq	?
};endstruct

struct DriveParameters
{
  .size 	dw	?
  .flags	dw	?
  .cylinders	dd	?
  .heads	dd	?
  .sectors	dd	?
  .totalSectors dq	?
  .sectorSize	dw	?
};endstruct

BIOS.DiskService		equ	0x13
BIOS.DiskService.ExtIsInstalled equ	0x41
BIOS.DiskService.ExtendedRead	equ	0x42
BIOS.DiskService.ExtDriveParams equ	0x48
