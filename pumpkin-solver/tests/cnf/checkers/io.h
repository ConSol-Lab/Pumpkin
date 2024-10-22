#include <stdio.h>

#ifdef _WIN32
int getc_unlocked(FILE *stream) {
  // On windows, I could not find a way to quickly implement this method
  // whilst avoiding locking. So, we lock. The performance will be
  // impacted slightly, so if it becomes an issue, this could be re-evaluated.
  return getc(stream);
}
#endif
