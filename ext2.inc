struct Ext2_Superblock
{
  .inodes_count dd ?
  .blocks_count dd ?
  .r_blocks_count dd ?
  .free_blocks_count dd ?
  .free_inodes_count dd ?
  .first_data_block dd ?
  .log_block_size dd ?
  .log_frag_size dd ?
  .blocks_per_group dd ?
  .frags_per_group dd ?
  .inodes_per_group dd ?
  .mtime dd ?
  .wtime dd ?
  .mnt_count dw ?
  .max_mnt_count dw ?
  .magic dw ?
  .state dw ?
  .errors dw ?
  .minor_rev_level dw ?
  .lastcheck dd ?
  .checkinterval dd ?
  .creator_os dd ?
  .rev_level dd ?
  .def_resuid dw ?
  .def_resgid dw ?
  .first_ino dd ?
  .inode_size dw ?
}

Ext2_Superblock_Magic = 0xEF53

struct Ext2_BlockGroupDescriptor
{
  .block_bitmap dd ?;
  .inode_bitmap dd ?;
  .inode_table dd ?;
  .free_blocks_count dw ?;
  .free_inodes_count dw ?;
  .used_dirs_count dw ?;
  .padding db 32 - ($ - .block_bitmap) dup ?;
}

struct Ext2_INode
{
  .mode dw ?;
  .uid dw ?;
  .size dd ?;
  .atime dd ?;
  .ctime dd ?;
  .mtime dd ?;
  .dtime dd ?;
  .gid dw ?;
  .links_count dw ?;
  .blocks dd ?;
  .flags dd ?;
  .osd1 dd ?;
  .block dd 12 dup ?;
  .blockIndir dd ?;
  .blockIndirDoub dd ?;
  .blockIndirTrip dd ?;
  .generaton dd ?;
  .file_acl dd ?;
  .dir_acl dd ?;
  .faddr dd ?;
  .osd2 dd ?, ?, ?;
}

Ext2_Bootloader_INode = 5
Ext2_LogMinBlockSize = 10
Ext2_LogBGDSize = 5

struct Ext2_Directory
{
  .inode dd ?;
  .rec_len dw ?;
  .name_len db ?;
  .file_type db ?;
  .name db ?;
}
