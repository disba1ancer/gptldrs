struct GPTHeader
{
  .magic			dq	?
  .revisionMinor		dw	?
  .revisionMajor		dw	?
  .size 			dd	?
  .checksum			dd	?
  .pad				dd	?
  .currentHeaderLBA		dq	?
  .alternativeHeaderLBA 	dq	?
  .minDataLBA			dq	?
  .maxDataLBA			dq	?
  .diskGUID			db 16 dup ?
  .partitionArrayStart		dq	?
  .partitionEntries		dd	?
  .partitionEntrySize		dd	?
  .partitionArrayChecksum	dd	?
}

struct GPTPartition
{
  .typeGUID	db 16 dup ?
  .partGUID	db 16 dup ?
  .firstLBA	dq	?
  .lastLBA	dq	?
  .attributes	dq	?
}
