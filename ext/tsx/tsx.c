/* TinyScheme Extensions
 * (c) 2002 Visual Tools, S.A.
 * Manuel Heras-Gilsanz (manuel@heras-gilsanz.com)
 *
 * This software is subject to the terms stated in the
 * LICENSE file.
 */

#include "miniscm.h"
#include <sys/stat.h>
#ifdef _WIN32
#include <ws2tcpip.h>
#include <time.h>
#include <sys/timeb.h>
#include <io.h>
#else
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <unistd.h>
#include <time.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <string.h>
#include <dirent.h>
#endif
#include "tsx.h"

#ifdef HAVE_MISC
pointer foreign_getenv(pointer args)
{
  pointer first_arg;
  pointer ret;
  char * varname;
  char * value;

  if(args == NIL)
  {
    return F;
  }

  first_arg = car(args);

  if(!is_string(first_arg))
  {
    return F;
  }

  varname = strvalue(first_arg);
  value = getenv(varname);
  if (0 == value)
  {
    ret = F;
  }
  else
  {
    ret = mk_string(value);
  }
  return ret;
}

pointer foreign_system(pointer args)
{
  pointer first_arg;
  char * command;
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_string(first_arg))
    return F;

  command = strvalue(first_arg);
  if(0 == command)
    return F;

  retcode = system(command);
  if( (127 == retcode) || (-1 == retcode) )
    return F;

  return (mk_integer(retcode));
}
#endif /* defined (HAVE_MISC) */

#ifdef HAVE_FILESYSTEM
pointer foreign_filesize(pointer args)
{
  pointer first_arg;
  pointer ret;
  struct stat buf;
  char * filename;
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_string(first_arg)) {
    return F;
  }

  filename = strvalue(first_arg);
  retcode = stat(filename, &buf);
  if (0 == retcode)
  {
    ret = mk_integer(buf.st_size);
  }
  else
  {
    ret = F;
  }
  return ret;
}

pointer foreign_fileexists(pointer args)
{
  pointer first_arg;
  pointer ret;
  struct stat buf;
  char * filename;
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_string(first_arg)) {
    return F;
  }

  filename = strvalue(first_arg);
  retcode = stat(filename, &buf);
  if (0 == retcode)
  {
    ret = T;
  }
  else
  {
    ret = F;
  }
  return ret;
}

pointer foreign_deletefile(pointer args)
{
  pointer first_arg;
  pointer ret;
  char * filename;
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_string(first_arg)) {
    return F;
  }

  filename = strvalue(first_arg);
#ifdef _WIN32
  retcode = _unlink(filename);
#else
  retcode = unlink(filename);
#endif
  if (0 == retcode) {
    ret = T;
  }
  else {
    ret = F;
  }
  return ret;
}

pointer foreign_opendirstream(pointer args)
{
  pointer first_arg;
  char * dirpath;
#ifdef _WIN32
  long dir;
  struct _finddata_t entry;
#else
  DIR * dir;
#endif

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_string(first_arg))
    return F;

  dirpath = strvalue(first_arg);

#ifdef _WIN32
  dir = _findfirst(dirpath, &entry);
  if(-1L == dir)
#else
  dir = opendir(dirpath);
  if(0 == dir)
#endif
    return F;

  return (mk_integer((int) dir));
}

pointer foreign_readdirentry(pointer args)
{
  pointer first_arg;
#ifdef _WIN32
  long dir;
  struct _finddata_t entry;
#else
  DIR * dir;
  struct dirent * entry;
#endif

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_number(first_arg))
    return F;

#ifdef _WIN32
  dir = (long) ivalue(first_arg);
  if(-1L == dir)
#else
  dir = (DIR *) ivalue(first_arg);
  if(0 == dir)
#endif
    return F;

#ifdef _WIN32
  dir = _findnext(dir, &entry);
  if(-1L == dir)
#else
  entry = readdir(dir);
  if(0 == entry)
#endif
    return EOF_OBJ;

#ifdef _WIN32
  return (mk_string(entry.name));
#else
  return (mk_string(entry->d_name));
#endif
}

pointer foreign_closedirstream(pointer args)
{
  pointer first_arg;
#ifdef _WIN32
  long dir;
#else
  DIR * dir;
#endif

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_number(first_arg))
    return F;

#ifdef _WIN32
  dir = (long) ivalue(first_arg);
  if(-1L == dir)
#else
  dir = (DIR *) ivalue(first_arg);
  if(0 == dir)
#endif
    return F;

#ifdef _WIN32
  _findclose(dir);
#else
  closedir(dir);
#endif
  return T;
}
#endif /* defined (HAVE_FILESYSTEM) */

#ifdef HAVE_TIME
pointer foreign_time(pointer args)
{
  time_t now;
  struct tm * now_tm;
  pointer ret;

  if(args != NIL)
  {
    return F;
  }

  time(&now);
  now_tm = localtime(&now);

  ret = cons(mk_integer((long) now_tm->tm_year),
         cons(mk_integer((long) now_tm->tm_mon),
          cons(mk_integer((long) now_tm->tm_mday),
           cons(mk_integer((long) now_tm->tm_hour),
            cons(mk_integer((long) now_tm->tm_min),
             cons(mk_integer((long) now_tm->tm_sec),NIL))))));

  return ret;
}

pointer foreign_gettimeofday(pointer args)
{
  struct timeval tv;
  pointer ret;

#ifdef _WIN32
  struct _timeb tb;

  _ftime(&tb);
  tv.tv_sec = (long) tb.time;
  tv.tv_usec = tb.millitm * 1000;
#else
  gettimeofday(&tv, 0);
#endif

  ret = cons(mk_integer((long) tv.tv_sec),
         cons(mk_integer((long) tv.tv_usec),
          NIL));

  return ret;
}

pointer foreign_usleep(pointer args)
{
  pointer first_arg;
  long usec;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_integer(first_arg)) {
    return F;
  }

  usec = ivalue(first_arg);
#ifdef _WIN32
  if (usec > 0) {
    HANDLE hTimer;
    LARGE_INTEGER dueTime;

    dueTime.QuadPart = -10LL * usec;

    hTimer = CreateWaitableTimer(NULL, TRUE, NULL);
    SetWaitableTimer(hTimer, &dueTime, 0, NULL, NULL, 0);
    WaitForSingleObject(hTimer, INFINITE);
    CloseHandle(hTimer);
  }
#else
  usleep(usec);
#endif

  return T;
}
#endif /* defined (HAVE_TIME) */

#ifdef HAVE_SOCKETS
pointer foreign_makeclientsocket(pointer args)
{
  pointer first_arg;
  pointer second_arg;
  pointer ret;
  struct sockaddr_in address;
  struct in_addr inaddr;
  struct hostent * host;
  char * hostname;
  int retcode;
  long port;
  int sock;
#ifdef _WIN32
  int size = sizeof(address);
#endif

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_string(first_arg)) {
    return F;
  }
  args = cdr(args);
  second_arg = car(args);
  if(!is_number(second_arg)) {
    return F;
  }

  hostname = strvalue(first_arg);
  port = ivalue(second_arg);

#ifdef _WIN32
  /* inet_pton() is not implemented in Windows XP, 2003 */
  if (WSAStringToAddress(hostname, AF_INET, NULL, (struct sockaddr *)&address, &size) == 0) {
    inaddr = address.sin_addr;
#else
  if(inet_aton(hostname, &inaddr)) {
#endif
    host = gethostbyaddr((char *) &inaddr, sizeof(inaddr), AF_INET);
  } else
    host = gethostbyname(hostname);

  if(0 == host) {
    return F;
  }

  sock = socket(PF_INET, SOCK_STREAM, 0);
  if(-1==sock) {
    return F;
  }

  address.sin_family = AF_INET;
  address.sin_port   = htons((u_short) port);
  memcpy(&address.sin_addr, host->h_addr_list[0], sizeof(address.sin_addr));

  retcode = connect(sock, (struct sockaddr *)&address, sizeof(address));
  if (0 == retcode) {
    ret = mk_integer(sock);
  }
  else {
    ret = F;
  }
  return ret;
}

pointer foreign_makeserversocket(pointer args)
{
  pointer first_arg;
  struct sockaddr_in address;
  long port;
#ifdef _WIN32
  const char one = 1;
#else
  int one = 1;
#endif
  int sock;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_number(first_arg)) {
    return F;
  }

  port = ivalue(first_arg);

  sock = socket(PF_INET, SOCK_STREAM, 0);
  if(-1==sock) {
    return F;
  }

  setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));

  address.sin_family = AF_INET;
  address.sin_port   = htons((u_short) port);
  memset(&address.sin_addr, 0, sizeof(address.sin_addr));

  if(bind(sock, (struct sockaddr *) &address, sizeof(address))) {
    return F;
  }

  if(listen(sock, 1)) {
    return F;
  }

  return (mk_integer(sock));
}

pointer foreign_recv(pointer args)
{
  pointer first_arg;
  pointer second_arg;
  int sock;
  char * buf;
  pointer ret;
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_number(first_arg)) {
    return F;
  }
  args = cdr(args);
  second_arg = car(args);
  if(!is_string(second_arg)) {
    return F;
  }

  sock = ivalue(first_arg);
  buf  = strvalue(second_arg);

  retcode = recv(sock, buf, strlen(buf), 0);
  if (-1 == retcode) {
    ret = F;
  }
  else {
    ret = mk_integer(retcode);
  }

  return ret;
}

pointer foreign_recvnewbuf(pointer args)
{
  pointer first_arg;
  int sock;
  pointer ret;
  int lenreceived;
  char buf[2500];

  if(args == NIL) return F;

  first_arg = car(args);
  if(!is_number(first_arg)) return F;

  sock = ivalue(first_arg);

  lenreceived = recv(sock, buf, sizeof(buf) - 1, 0);
  if (-1 == lenreceived) return F;

  buf[lenreceived] = 0;
  ret = mk_string(buf);

  return ret;
}

pointer foreign_isdataready(pointer args)
{
  pointer first_arg;
  int sock;
  struct timeval tv;
  fd_set fds;
  fd_set fdsin;

  if(args == NIL) return F;

  first_arg = car(args);
  if(!is_number(first_arg)) return F;

  sock = ivalue(first_arg);

  tv.tv_sec = 0;
  tv.tv_usec = 0;

  FD_ZERO(&fds);
  FD_SET(sock, &fds);
  fdsin = fds;
  if (select(1+sock, &fdsin, NULL, NULL, &tv) < 0)
    {
      return F;
    }
  if (FD_ISSET(sock, &fdsin))
    return T;
  return F;
}

pointer foreign_sockpeek(pointer args)
{
  pointer first_arg;
  int sock;
  pointer ret;
  int lenreceived;
  char buf[2500];

  if(args == NIL) return F;

  first_arg = car(args);
  if(!is_number(first_arg)) return F;

  sock = ivalue(first_arg);

  lenreceived = recv(sock, buf, sizeof(buf) - 1, MSG_PEEK);
  if (-1 == lenreceived) return F;

  buf[lenreceived] = 0;
  ret = mk_string(buf);

  return ret;
}

pointer foreign_send(pointer args)
{
  pointer first_arg;
  pointer second_arg;
  int sock;
  char * buf;
  pointer ret;
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_number(first_arg)) {
    return F;
  }
  args = cdr(args);
  second_arg = car(args);
  if(!is_string(second_arg)) {
    return F;
  }

  sock = ivalue(first_arg);
  buf  = strvalue(second_arg);

  retcode = send(sock, buf, strlen(buf), 0);
  if (-1 == retcode) {
    ret = F;
  }
  else {
    ret = mk_integer(retcode);
  }

  return ret;
}

pointer foreign_accept(pointer args)
{
  pointer first_arg;
  int sock;
  struct sockaddr_in addr;
  pointer ret;
  socklen_t addr_len = sizeof(struct sockaddr_in);
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_number(first_arg)) {
    return F;
  }

  sock = ivalue(first_arg);

  retcode = accept(sock, (struct sockaddr *)&addr, &addr_len);
  if (-1 == retcode) {
    ret = F;
  }
  else {
    ret = mk_integer(retcode);
  }

  return ret;
}

pointer foreign_closesocket(pointer args)
{
  pointer first_arg;
  int sock;
  int retcode;

  if(args == NIL)
    return F;

  first_arg = car(args);
  if(!is_number(first_arg))
    return F;

  sock = ivalue(first_arg);

#ifdef _WIN32
  retcode = _close(sock);
#else
  retcode = close(sock);
#endif
  if (-1 == retcode)
    return F;

  return T;
}
#endif /* defined (HAVE_SOCKETS) */


/* This function gets called when MiniScheme is loading the extension */
void init_tsx(void)
{
#ifdef HAVE_MISC
  scheme_register_foreign_func("getenv", foreign_getenv);
  scheme_register_foreign_func("system", foreign_system);
#endif /* defined (HAVE_MISC) */
#ifdef HAVE_TIME
  scheme_register_foreign_func("time", foreign_time);
  scheme_register_foreign_func("gettimeofday", foreign_gettimeofday);
  scheme_register_foreign_func("usleep", foreign_usleep);
#endif /* defined (HAVE_TIME) */
#ifdef HAVE_FILESYSTEM
  scheme_register_foreign_func("file-size", foreign_filesize);
  scheme_register_foreign_func("file-exists?", foreign_fileexists);
  scheme_register_foreign_func("delete-file", foreign_deletefile);
  scheme_register_foreign_func("open-dir-stream", foreign_opendirstream);
  scheme_register_foreign_func("read-dir-entry", foreign_readdirentry);
  scheme_register_foreign_func("close-dir-stream", foreign_closedirstream);
#endif /* defined (HAVE_FILESYSTEM) */
#ifdef HAVE_SOCKETS
  scheme_register_foreign_func("make-client-socket", foreign_makeclientsocket);
  scheme_register_foreign_func("make-server-socket", foreign_makeserversocket);
  scheme_register_foreign_func("recv!", foreign_recv);
  scheme_register_foreign_func("recv-new-string", foreign_recvnewbuf);
  scheme_register_foreign_func("sock-peek", foreign_sockpeek);
  scheme_register_foreign_func("sock-is-data-ready?", foreign_isdataready);
  scheme_register_foreign_func("send", foreign_send);
  scheme_register_foreign_func("accept", foreign_accept);
  scheme_register_foreign_func("close-socket", foreign_closesocket);
#endif /* defined (HAVE_SOCKETS) */
}
