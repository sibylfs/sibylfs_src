#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <errno.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <dirent.h>

int main() {
  int err;

  mkdir("dmz",0700);
  chdir("dmz");
  errno = 0;
  err = rmdir("../dmz");
  if (err == -1) printf("rmdir error: %s\n",strerror(errno));
  open("bar",O_CREAT | O_RDONLY,0600);

  return 0;
}
