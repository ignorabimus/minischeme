/* TinyScheme Extensions
 * (c) 2002 Visual Tools, S.A.
 * Manuel Heras-Gilsanz (manuel@heras-gilsanz.com)
 *
 * This software is subject to the terms stated in the
 * LICENSE file.
 */

#include "scheme-private.h"
#include <sys/stat.h>
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
#include "tsx.h"

#undef cons

#ifdef HAVE_MISC
pointer foreign_getenv(scheme * sc, pointer args)
{
  pointer first_arg;
  pointer ret;
  char * varname;
  char * value;

  if(args == sc->NIL)
  {
    return sc->F;
  }

  first_arg = sc->vptr->pair_car(args);

  if(!sc->vptr->is_string(first_arg))
  {
    return sc->F;
  }

  varname = sc->vptr->string_value(first_arg);
  value = getenv(varname);
  if (0 == value)
  {
    ret = sc->F;
  }
  else
  {
    ret = sc->vptr->mk_string(sc,value);
  }
  return ret;
}

pointer foreign_system(scheme * sc, pointer args)
{
  pointer first_arg;
  char * command;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(first_arg))
    return sc->F;
  
  command = sc->vptr->string_value(first_arg);
  if(0 == command)
    return sc->F;

  retcode = system(command);
  if( (127 == retcode) || (-1 == retcode) )
    return sc->F;

  return (sc->vptr->mk_integer(sc,retcode));
}
#endif /* defined (HAVE_MISC) */

#ifdef HAVE_FILESYSTEM
pointer foreign_filesize(scheme * sc, pointer args)
{
  pointer first_arg;
  pointer ret;
  struct stat buf;
  char * filename;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(first_arg)) {
    return sc->F;
  }

  filename = sc->vptr->string_value(first_arg);
  retcode = stat(filename, &buf);
  if (0 == retcode)
  {
    ret = sc->vptr->mk_integer(sc,buf.st_size);
  }
  else
  {
    ret = sc->F;
  }
  return ret;
}

pointer foreign_fileexists(scheme * sc, pointer args)
{
  pointer first_arg;
  pointer ret;
  struct stat buf;
  char * filename;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(first_arg)) {
    return sc->F;
  }

  filename = sc->vptr->string_value(first_arg);
  retcode = stat(filename, &buf);
  if (0 == retcode)
  {
    ret = sc->T;
  }
  else
  {
    ret = sc->F;
  }
  return ret;
}

pointer foreign_deletefile(scheme * sc, pointer args)
{
  pointer first_arg;
  pointer ret;
  char * filename;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(first_arg)) {
    return sc->F;
  }

  filename = sc->vptr->string_value(first_arg);
  retcode = unlink(filename);
  if (0 == retcode) {
    ret = sc->T;
  }
  else {
    ret = sc->F;
  }
  return ret;
}

pointer foreign_opendirstream(scheme * sc, pointer args)
{
  pointer first_arg;
  char * dirpath;
  DIR * dir;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(first_arg))
    return sc->F;
  
  dirpath = sc->vptr->string_value(first_arg);

  dir = opendir(dirpath);
  if(0 == dir)
    return sc->F;

  return (sc->vptr->mk_integer(sc,(int) dir));
}

pointer foreign_readdirentry(scheme * sc, pointer args)
{
  pointer first_arg;
  DIR * dir;
  struct dirent * entry;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg))
    return sc->F;
  
  dir = (DIR *) sc->vptr->ivalue(first_arg);
  if(0 == dir)
    return sc->F;

  entry = readdir(dir);
  if(0 == entry)
    return sc->EOF_OBJ;

  return (sc->vptr->mk_string(sc,entry->d_name));
}

pointer foreign_closedirstream(scheme * sc, pointer args)
{
  pointer first_arg;
  DIR * dir;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg))
    return sc->F;
  
  dir = (DIR *) sc->vptr->ivalue(first_arg);
  if(0 == dir)
    return sc->F;

  closedir(dir);
  return sc->T;
}
#endif /* defined (HAVE_FILESYSTEM) */

#ifdef HAVE_TIME
pointer foreign_time(scheme * sc, pointer args)
{
  time_t now;
  struct tm * now_tm;
  pointer ret;

  if(args != sc->NIL)
  {
    return sc->F;
  }

  time(&now);
  now_tm = localtime(&now);
  
  ret = sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) now_tm->tm_year),
         sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) now_tm->tm_mon),
          sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) now_tm->tm_mday),
           sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) now_tm->tm_hour),
            sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) now_tm->tm_min),
             sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) now_tm->tm_sec),sc->NIL))))));

  return ret;
}

pointer foreign_gettimeofday(scheme * sc, pointer args)
{
  struct timeval tv;
  pointer ret;

  gettimeofday(&tv, 0);
  
  ret = sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) tv.tv_sec),
         sc->vptr->cons(sc,sc->vptr->mk_integer(sc,(long) tv.tv_usec),
          sc->NIL));

  return ret;
}

pointer foreign_usleep(scheme * sc, pointer args)
{
  pointer first_arg;
  long usec;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_integer(first_arg)) {
    return sc->F;
  }

  usec = sc->vptr->ivalue(first_arg);
  retcode = usleep(usec);

  return sc->T;
}
#endif /* defined (HAVE_TIME) */

#ifdef HAVE_SOCKETS
pointer foreign_makeclientsocket(scheme * sc, pointer args)
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

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(first_arg)) {
    return sc->F;
  }
  args = sc->vptr->pair_cdr(args);
  second_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(second_arg)) {
    return sc->F;
  }
  
  hostname = sc->vptr->string_value(first_arg);
  port = sc->vptr->ivalue(second_arg);

  if(inet_aton(hostname, &inaddr))
    host = gethostbyaddr((char *) &inaddr, sizeof(inaddr), AF_INET);
  else
    host = gethostbyname(hostname);

  if(0 == host) {
    return sc->F;
  }

  sock = socket(PF_INET, SOCK_STREAM, 0);
  if(-1==sock) {
    return sc->F;
  }

  address.sin_family = AF_INET;
  address.sin_port   = htons(port);
  memcpy(&address.sin_addr, host->h_addr_list[0], sizeof(address.sin_addr));

  retcode = connect(sock, (struct sockaddr *)&address, sizeof(address));
  if (0 == retcode) {
    ret = sc->vptr->mk_integer(sc,sock);
  }
  else {
    ret = sc->F;
  }
  return ret;
}

pointer foreign_makeserversocket(scheme * sc, pointer args)
{
  pointer first_arg;
  struct sockaddr_in address;
  long port;
  int one = 1;
  int sock;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg)) {
    return sc->F;
  }
  
  port = sc->vptr->ivalue(first_arg);

  sock = socket(PF_INET, SOCK_STREAM, 0);
  if(-1==sock) {
    return sc->F;
  }

  setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));

  address.sin_family = AF_INET;
  address.sin_port   = htons(port);
  memset(&address.sin_addr, 0, sizeof(address.sin_addr));

  if(bind(sock, (struct sockaddr *) &address, sizeof(address))) {
    return sc->F;
  }

  if(listen(sock, 1)) {
    return sc->F;
  }

  return (sc->vptr->mk_integer(sc,sock));
}

pointer foreign_recv(scheme * sc, pointer args)
{
  pointer first_arg;
  pointer second_arg;
  int sock;
  char * buf;
  pointer ret;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg)) {
    return sc->F;
  }
  args = sc->vptr->pair_cdr(args);
  second_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(second_arg)) {
    return sc->F;
  }
  
  sock = sc->vptr->ivalue(first_arg);
  buf  = sc->vptr->string_value(second_arg);

  retcode = recv(sock, buf, strlen(buf), 0);
  if (-1 == retcode) {
    ret = sc->F;
  }
  else {
    ret = sc->vptr->mk_integer(sc,retcode);
  }

  return ret;
}

pointer foreign_recvnewbuf(scheme * sc, pointer args)
{
  pointer first_arg;
  int sock;
  pointer ret;
  int lenreceived;
  char buf[2500];

  if(args == sc->NIL) return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg)) return sc->F;
  
  sock = sc->vptr->ivalue(first_arg);

  lenreceived = recv(sock, buf, sizeof(buf) - 1, 0);
  if (-1 == lenreceived) return sc->F;

  buf[lenreceived] = 0;
  ret = sc->vptr->mk_string(sc,buf);

  return ret;
}

pointer foreign_isdataready(scheme * sc, pointer args)
{
  pointer first_arg;
  int sock;
  struct timeval tv;
  fd_set fds;
  fd_set fdsin;

  if(args == sc->NIL) return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg)) return sc->F;
  
  sock = sc->vptr->ivalue(first_arg);

  tv.tv_sec = 0;
  tv.tv_usec = 0;
  
  FD_ZERO(&fds);
  FD_SET(sock, &fds);
  fdsin = fds;
  if (select(1+sock, &fdsin, NULL, NULL, &tv) < 0)
    {
      return sc->F;
    }
  if (FD_ISSET(sock, &fdsin))
	  return sc->T;
  return sc->F;
}

pointer foreign_sockpeek(scheme * sc, pointer args)
{
  pointer first_arg;
  int sock;
  pointer ret;
  int lenreceived;
  char buf[2500];

  if(args == sc->NIL) return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg)) return sc->F;
  
  sock = sc->vptr->ivalue(first_arg);

  lenreceived = recv(sock, buf, sizeof(buf) - 1, MSG_PEEK);
  if (-1 == lenreceived) return sc->F;

  buf[lenreceived] = 0;
  ret = sc->vptr->mk_string(sc,buf);

  return ret;
}

pointer foreign_send(scheme * sc, pointer args)
{
  pointer first_arg;
  pointer second_arg;
  int sock;
  char * buf;
  pointer ret;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg)) {
    return sc->F;
  }
  args = sc->vptr->pair_cdr(args);
  second_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_string(second_arg)) {
    return sc->F;
  }
  
  sock = sc->vptr->ivalue(first_arg);
  buf  = sc->vptr->string_value(second_arg);

  retcode = send(sock, buf, strlen(buf), 0);
  if (-1 == retcode) {
    ret = sc->F;
  }
  else {
    ret = sc->vptr->mk_integer(sc,retcode);
  }

  return ret;
}

pointer foreign_accept(scheme * sc, pointer args)
{
  pointer first_arg;
  int sock;
  struct sockaddr_in addr;
  pointer ret;
  socklen_t addr_len = sizeof(struct sockaddr_in);
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg)) {
    return sc->F;
  }
  
  sock = sc->vptr->ivalue(first_arg);

  retcode = accept(sock, (struct sockaddr *)&addr, &addr_len);
  if (-1 == retcode) {
    ret = sc->F;
  }
  else {
    ret = sc->vptr->mk_integer(sc,retcode);
  }

  return ret;
}

pointer foreign_closesocket(scheme * sc, pointer args)
{
  pointer first_arg;
  int sock;
  int retcode;

  if(args == sc->NIL)
    return sc->F;

  first_arg = sc->vptr->pair_car(args);
  if(!sc->vptr->is_number(first_arg))
    return sc->F;
  
  sock = sc->vptr->ivalue(first_arg);

  retcode = close(sock);
  if (-1 == retcode)
    return sc->F;

  return sc->T;
}
#endif /* defined (HAVE_SOCKETS) */


/* This function gets called when TinyScheme is loading the extension */
void init_tsx (scheme * sc)
{
#ifdef HAVE_MISC
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"getenv"),
                               sc->vptr->mk_foreign_func(sc, foreign_getenv));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"system"),
                               sc->vptr->mk_foreign_func(sc, foreign_system));
#endif /* defined (HAVE_MISC) */
#ifdef HAVE_TIME
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"time"),
                               sc->vptr->mk_foreign_func(sc, foreign_time));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"gettimeofday"),
                               sc->vptr->mk_foreign_func(sc, foreign_gettimeofday));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"usleep"),
                               sc->vptr->mk_foreign_func(sc, foreign_usleep));
#endif /* defined (HAVE_TIME) */
#ifdef HAVE_FILESYSTEM
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"file-size"),
                               sc->vptr->mk_foreign_func(sc, foreign_filesize));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"file-exists?"),
                               sc->vptr->mk_foreign_func(sc, foreign_fileexists));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"delete-file"),
                               sc->vptr->mk_foreign_func(sc, foreign_deletefile));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"open-dir-stream"),
                               sc->vptr->mk_foreign_func(sc, foreign_opendirstream));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"read-dir-entry"),
                               sc->vptr->mk_foreign_func(sc, foreign_readdirentry));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"close-dir-stream"),
                               sc->vptr->mk_foreign_func(sc, foreign_closedirstream));
#endif /* defined (HAVE_FILESYSTEM) */
#ifdef HAVE_SOCKETS
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"make-client-socket"),
                               sc->vptr->mk_foreign_func(sc, foreign_makeclientsocket));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"make-server-socket"),
                               sc->vptr->mk_foreign_func(sc, foreign_makeserversocket));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"recv!"),
                               sc->vptr->mk_foreign_func(sc, foreign_recv));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"recv-new-string"),
                               sc->vptr->mk_foreign_func(sc, foreign_recvnewbuf));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"sock-peek"),
                               sc->vptr->mk_foreign_func(sc, foreign_sockpeek));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"sock-is-data-ready?"),
                               sc->vptr->mk_foreign_func(sc, foreign_isdataready));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"send"),
                               sc->vptr->mk_foreign_func(sc, foreign_send));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"accept"),
                               sc->vptr->mk_foreign_func(sc, foreign_accept));
  sc->vptr->scheme_define(sc,sc->global_env,
                               sc->vptr->mk_symbol(sc,"close-socket"),
                               sc->vptr->mk_foreign_func(sc, foreign_closesocket));
#endif /* defined (HAVE_SOCKETS) */
}
