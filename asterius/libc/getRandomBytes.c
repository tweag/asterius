/*
Copyright Bryan O'Sullivan 2012

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Johan Tibell nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "MachDeps.h"

int hashable_getRandomBytes(unsigned char *dest, int nbytes);

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)

#include <windows.h>
#include <wincrypt.h>

int hashable_getRandomBytes(unsigned char *dest, int nbytes)
{
  HCRYPTPROV hCryptProv;
  int ret;

  if (!CryptAcquireContextA(&hCryptProv, NULL, NULL, PROV_RSA_FULL,
			    CRYPT_VERIFYCONTEXT))
    return -1;

  ret = CryptGenRandom(hCryptProv, (DWORD) nbytes, (BYTE *) dest) ? nbytes : -1;

  CryptReleaseContext(hCryptProv, 0);

 bail:
  return ret;
}

#else

#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

/* Assumptions: /dev/urandom exists and does something sane, and does
   not block. */

int hashable_getRandomBytes(unsigned char *dest, int nbytes)
{
  ssize_t off, nread;
  int fd;

  fd = open("/dev/urandom", O_RDONLY);
  if (fd == -1)
    return -1;

  for (off = 0; nbytes > 0; nbytes -= nread) {
    nread = read(fd, dest + off, nbytes);
    off += nread;
    if (nread == -1) {
      off = -1;
      break;
    }
  }

 bail:
  close(fd);

  return off;
}

#endif
