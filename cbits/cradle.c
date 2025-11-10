#ifdef __linux__
#define _GNU_SOURCE
#endif
#include <fcntl.h>
#include <unistd.h>

int cloexec_pipe(int pipefd[2]) {
#ifdef __linux__
  int r = pipe2(pipefd, O_CLOEXEC);
#else
  int r = pipe(pipefd);
  r = fcntl(pipefd[0], F_SETFD, FD_CLOEXEC);
  r = fcntl(pipefd[1], F_SETFD, FD_CLOEXEC);
#endif
  return r;
}
