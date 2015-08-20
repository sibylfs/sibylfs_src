#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>

int write_sz = 4096*4096*4;

int main() {
  long* page;
  int fd, i, j, sz;
  struct stat st;

  page = (long *)malloc(write_sz);

  for (j = 0; j < 32; j++) {
    errno = 0;
    fd = open("space_64M",O_CREAT | O_WRONLY,0600);
    if (fd == -1) {
      printf("open error: %s\n",strerror(errno));
    }

    errno = 0;
    sz = write(fd,page,write_sz);
    if (sz == -1) {
      printf("write error: %s\n",strerror(errno));
    } else if (sz != write_sz) {
      printf("write only %d\n",sz);
    }
    close(fd);

    link("space_64M","space_64M_hardlink");
    link("space_64M_hardlink","space_64M_hardlink2");

    fd = open("empty",O_CREAT,0600);
    close(fd);
    rename("empty","space_64M");
    rename("space_64M","empty");
    rename("empty","space_64M_hardlink");
    rename("space_64M_hardlink","empty");

    stat("space_64M_hardlink2",&st);

    printf("nlink = %d\n",(int)st.st_nlink);

    fd = open("empty",O_CREAT,0600);
    close(fd);

    rename("empty","space_64M_hardlink2");
    rename("space_64M_hardlink2","empty");
  }

  return 0;
}
