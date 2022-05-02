# gptldrs
Simple leagacy bootloader set for GPT disks.
There is currently a main bootloader and an ext2 partition bootloader, described below.

### Main bootloader
Main bootloader loads the first sector of active GPT partition.
The active partition is determined by the 3rd bit of the partition attribute flags.
Main bootloader stores pointer to GPTPartition structure (see [gpt.inc](./gpt.inc)) to `DS:SI` before control transfer to partition bootloader.

### Ext2 partition bootloader
Ext2 partition bootloader loads special bootloader INode (`EXT2_BOOT_LOADER_INO` that has number `5`) at address `0x1000:0`.
Just like the main bootloader, this bootloader stores a pointer to the GPTPartition structure in `DS:SI`
