
#include <stddef.h>

int shell_forth_command(int argc, const char** argv);

typedef struct FAT_PFile {
	struct FAT_file* file;
	size_t pos;
} FAT_PFile_t;
