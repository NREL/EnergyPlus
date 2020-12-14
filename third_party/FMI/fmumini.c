///////////////////////////////////////////////////////
/// \file   fmumini.c
///
/// \brief  unzip functions
///
/// \author Wangda Zuo
///         Simulation Research Group,
///         LBNL,
///         WZuo@lbl.gov
///
/// \date   2012-02-02
///
///
/// This file provides unzip functions for fmus and it is modified
/// based on the miniunz.c (Version 1.1) from the MiniZip project.
/// The original file, its copyright notice and its license can
/// be found in FMI/Copyright folder
///////////////////////////////////////////////////////
/*
   fmumini.c
   Version 1.1, February 14h, 2010
   sample part of the MiniZip project - ( http://www.winimage.com/zLibDll/minizip.html )

         Copyright (C) 1998-2010 Gilles Vollant (minizip) ( http://www.winimage.com/zLibDll/minizip.html )

         Modifications of Unzip for Zip64
         Copyright (C) 2007-2008 Even Rouault

         Modifications for Zip64 support on both zip and unzip
         Copyright (C) 2009-2010 Mathias Svensson ( http://result42.com )
*/

#ifndef _WIN32
#ifndef __USE_FILE_OFFSET64
#define __USE_FILE_OFFSET64
#endif
#ifndef __USE_LARGEFILE64
#define __USE_LARGEFILE64
#endif
#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE
#endif
#ifndef _FILE_OFFSET_BIT
#define _FILE_OFFSET_BIT 64
#endif
#endif

// fopen is 64 bit by default on apple so fopen is fopen64
// linker errors occur by using fopen64
#ifdef __APPLE__
#define off64_t off_t
#define fopen64 fopen
#endif

#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>

#ifdef _WIN32
#include <direct.h>
#include <io.h>
#else
#include <unistd.h>
#include <utime.h>
#endif

#include "util.h"
#include <zlib/contrib/minizip/unzip.h>

#define CASESENSITIVITY (0)
#define WRITEBUFFERSIZE (8192)
#define MAXFILENAME (256)

#ifdef _WIN32
#ifndef __MINGW32__
#define USEWIN32IOAPI
#include <zlib/contrib/minizip/iowin32.h>
#endif
#endif
/*
  mini unzip, demo of unzip package

  usage :
  Usage : miniunz [-exvlo] file.zip [file_to_extract] [-d extractdir]

  list the file in the zipfile, and print the content of FILE_ID.ZIP or README.TXT
    if it exists
*/

/* change_file_date : change the date/time of a file
    filename : the filename of the file where date/time must be modified
    dosdate : the new date at the MSDos format (4 bytes)
    tmu_date : the SAME new date at the tm_unz format */
void change_file_date(filename, dosdate, tmu_date) const char *filename;
uLong dosdate;
tm_unz tmu_date;
{
#ifdef _WIN32
    HANDLE hFile;
    FILETIME ftm, ftLocal, ftCreate, ftLastAcc, ftLastWrite;

    hFile = CreateFileA(filename, GENERIC_READ | GENERIC_WRITE, 0, NULL, OPEN_EXISTING, 0, NULL);
    GetFileTime(hFile, &ftCreate, &ftLastAcc, &ftLastWrite);
    DosDateTimeToFileTime((WORD)(dosdate >> 16), (WORD)dosdate, &ftLocal);
    LocalFileTimeToFileTime(&ftLocal, &ftm);
    SetFileTime(hFile, &ftm, &ftLastAcc, &ftm);
    CloseHandle(hFile);
#else
#ifdef unix
    struct utimbuf ut;
    struct tm newdate;
    newdate.tm_sec = tmu_date.tm_sec;
    newdate.tm_min = tmu_date.tm_min;
    newdate.tm_hour = tmu_date.tm_hour;
    newdate.tm_mday = tmu_date.tm_mday;
    newdate.tm_mon = tmu_date.tm_mon;
    if (tmu_date.tm_year > 1900)
        newdate.tm_year = tmu_date.tm_year - 1900;
    else
        newdate.tm_year = tmu_date.tm_year;
    newdate.tm_isdst = -1;

    ut.actime = ut.modtime = mktime(&newdate);
    utime(filename, &ut);
#else
    (void)filename;
    (void)dosdate;
    (void)tmu_date;
#endif
#endif
}

/* mymkdir and change_file_date are not 100 % portable
   As I don't know well Unix, I wait feedback for the unix portion */

int mymkdir(dirname) const char *dirname;
{
    int ret = 0;
#ifdef _WIN32
    ret = _mkdir(dirname);
#else
#ifdef unix
    ret = mkdir(dirname, 0775);
#else
    (void)dirname;
#endif
#endif
    return ret;
}

int makedir(newdir) const char *newdir;
{
    char *buffer;
    char *p;
    int len = (int)strlen(newdir);

    if (len <= 0) return 0;

    buffer = (char *)malloc(len + 1);
    if (buffer == NULL) {
        printf("Error allocating memory\n");
        return UNZ_INTERNALERROR;
    }
    strcpy(buffer, newdir);

    if (buffer[len - 1] == '/') {
        buffer[len - 1] = '\0';
    }
    if (mymkdir(buffer) == 0) {
        free(buffer);
        return 1;
    }

    p = buffer + 1;
    while (1) {
        char hold;

        while (*p && *p != '\\' && *p != '/')
            p++;
        hold = *p;
        *p = 0;
        if ((mymkdir(buffer) == -1) && (errno == ENOENT)) {
            printf("couldn't create directory %s\n", buffer);
            free(buffer);
            return 0;
        }
        if (hold == 0) break;
        *p++ = hold;
    }
    free(buffer);
    return 1;
}

void do_banner()
{
    printf("MiniUnz 1.01b, demo of zLib + Unz package written by Gilles Vollant\n");
    printf("more info at http://www.winimage.com/zLibDll/unzip.html\n\n");
}

void do_help()
{
    printf("Usage : miniunz [-e] [-x] [-v] [-l] [-o] [-p password] file.zip [file_to_extr.] [-d extractdir]\n\n"
           "  -e  Extract without pathname (junk paths)\n"
           "  -x  Extract with pathname\n"
           "  -v  list files\n"
           "  -l  list files\n"
           "  -d  directory to extract into\n"
           "  -o  overwrite files without prompting\n"
           "  -p  extract crypted file using password\n\n");
}

void Display64BitsSize(ZPOS64_T n, int size_char)
{
    /* to avoid compatibility problem , we do here the conversion */
    char number[21];
    int offset = 19;
    int pos_string = 19;
    number[20] = 0;
    for (;;) {
        number[offset] = (char)((n % 10) + '0');
        if (number[offset] != '0') pos_string = offset;
        n /= 10;
        if (offset == 0) break;
        offset--;
    }
    {
        int size_display_string = 19 - pos_string;
        while (size_char > size_display_string) {
            size_char--;
            printf(" ");
        }
    }

    printf("%s", &number[pos_string]);
}

int do_list(uf) unzFile uf;
{
    uLong i;
    unz_global_info64 gi;
    int err;

    err = unzGetGlobalInfo64(uf, &gi);
    if (err != UNZ_OK) printf("error %d with zipfile in unzGetGlobalInfo \n", err);
    printf("  Length  Method     Size Ratio   Date    Time   CRC-32     Name\n");
    printf("  ------  ------     ---- -----   ----    ----   ------     ----\n");
    for (i = 0; i < gi.number_entry; i++) {
        char filename_inzip[256];
        unz_file_info64 file_info;
        uLong ratio = 0;
        const char *string_method;
        char charCrypt = ' ';
        err = unzGetCurrentFileInfo64(uf, &file_info, filename_inzip, sizeof(filename_inzip), NULL, 0, NULL, 0);
        if (err != UNZ_OK) {
            printf("error %d with zipfile in unzGetCurrentFileInfo\n", err);
            break;
        }
        if (file_info.uncompressed_size > 0) ratio = (uLong)((file_info.compressed_size * 100) / file_info.uncompressed_size);

        /* display a '*' if the file is crypted */
        if ((file_info.flag & 1) != 0) charCrypt = '*';

        if (file_info.compression_method == 0)
            string_method = "Stored";
        else if (file_info.compression_method == Z_DEFLATED) {
            uInt iLevel = (uInt)((file_info.flag & 0x6) / 2);
            if (iLevel == 0)
                string_method = "Defl:N";
            else if (iLevel == 1)
                string_method = "Defl:X";
            else if ((iLevel == 2) || (iLevel == 3))
                string_method = "Defl:F"; /* 2:fast , 3 : extra fast*/
        } else if (file_info.compression_method == Z_BZIP2ED) {
            string_method = "BZip2 ";
        } else
            string_method = "Unkn. ";

        Display64BitsSize(file_info.uncompressed_size, 7);
        printf("  %6s%c", string_method, charCrypt);
        Display64BitsSize(file_info.compressed_size, 7);
        printf(" %3lu%%  %2.2lu-%2.2lu-%2.2lu  %2.2lu:%2.2lu  %8.8lx   %s\n",
               ratio,
               (uLong)file_info.tmu_date.tm_mon + 1,
               (uLong)file_info.tmu_date.tm_mday,
               (uLong)file_info.tmu_date.tm_year % 100,
               (uLong)file_info.tmu_date.tm_hour,
               (uLong)file_info.tmu_date.tm_min,
               (uLong)file_info.crc,
               filename_inzip);
        if ((i + 1) < gi.number_entry) {
            err = unzGoToNextFile(uf);
            if (err != UNZ_OK) {
                printf("error %d with zipfile in unzGoToNextFile\n", err);
                break;
            }
        }
    }

    return 0;
}

int do_extract_currentfile(uf, popt_extract_without_path, popt_overwrite, password) unzFile uf;
const int *popt_extract_without_path;
int *popt_overwrite;
const char *password;
{
    char filename_inzip[256];
    char *filename_withoutpath;
    char *p;
    int err = UNZ_OK;
    FILE *fout = NULL;
    void *buf;
    uInt size_buf;

    unz_file_info64 file_info;
    err = unzGetCurrentFileInfo64(uf, &file_info, filename_inzip, sizeof(filename_inzip), NULL, 0, NULL, 0);

    if (err != UNZ_OK) {
        printf("error %d with zipfile in unzGetCurrentFileInfo\n", err);
        return err;
    }

    size_buf = WRITEBUFFERSIZE;
    buf = (void *)malloc(size_buf);
    if (buf == NULL) {
        printf("Error allocating memory\n");
        return UNZ_INTERNALERROR;
    }

    p = filename_withoutpath = filename_inzip;
    while ((*p) != '\0') {
        if (((*p) == '/') || ((*p) == '\\')) filename_withoutpath = p + 1;
        p++;
    }

    if ((*filename_withoutpath) == '\0') {
        if ((*popt_extract_without_path) == 0) {
            // printf("creating directory: %s\n",filename_inzip);
            mymkdir(filename_inzip);
        }
    } else {
        const char *write_filename;
        int skip = 0;

        if ((*popt_extract_without_path) == 0)
            write_filename = filename_inzip;
        else
            write_filename = filename_withoutpath;

        err = unzOpenCurrentFilePassword(uf, password);
        if (err != UNZ_OK) {
            printf("error %d with zipfile in unzOpenCurrentFilePassword\n", err);
        }

        if (((*popt_overwrite) == 0) && (err == UNZ_OK)) {
            char rep = 0;
            FILE *ftestexist;
            ftestexist = fopen64(write_filename, "rb");
            if (ftestexist != NULL) {
                fclose(ftestexist);
                do {
                    char answer[128];
                    int ret;

                    printf("The file %s exists. Overwrite ? [y]es, [n]o, [A]ll: ", write_filename);
                    ret = scanf("%1s", answer);
                    if (ret != 1) {
                        exit(EXIT_FAILURE);
                    }
                    rep = answer[0];
                    if ((rep >= 'a') && (rep <= 'z')) rep -= 0x20;
                } while ((rep != 'Y') && (rep != 'N') && (rep != 'A'));
            }

            if (rep == 'N') skip = 1;

            if (rep == 'A') *popt_overwrite = 1;
        }

        if ((skip == 0) && (err == UNZ_OK)) {
            fout = fopen64(write_filename, "wb");

            /* some zipfile don't contain directory alone before file */
            if ((fout == NULL) && ((*popt_extract_without_path) == 0) && (filename_withoutpath != (char *)filename_inzip)) {
                char c = *(filename_withoutpath - 1);
                *(filename_withoutpath - 1) = '\0';
                makedir(write_filename);
                *(filename_withoutpath - 1) = c;
                fout = fopen64(write_filename, "wb");
            }

            if (fout == NULL) {
                printf("error opening %s\n", write_filename);
            }
        }

        if (fout != NULL) {
            // printf(" extracting: %s\n",write_filename);

            do {
                err = unzReadCurrentFile(uf, buf, size_buf);
                if (err < 0) {
                    printf("error %d with zipfile in unzReadCurrentFile\n", err);
                    break;
                }
                if (err > 0)
                    if (fwrite(buf, err, 1, fout) != 1) {
                        printf("error in writing extracted file\n");
                        err = UNZ_ERRNO;
                        break;
                    }
            } while (err > 0);
            if (fout) fclose(fout);

            if (err == 0) change_file_date(write_filename, file_info.dosDate, file_info.tmu_date);
        }

        if (err == UNZ_OK) {
            err = unzCloseCurrentFile(uf);
            if (err != UNZ_OK) {
                printf("error %d with zipfile in unzCloseCurrentFile\n", err);
            }
        } else
            unzCloseCurrentFile(uf); /* don't lose the error */
    }

    free(buf);
    return err;
}

int do_extract(uf, opt_extract_without_path, opt_overwrite, password) unzFile uf;
int opt_extract_without_path;
int opt_overwrite;
const char *password;
{
    uLong i;
    unz_global_info64 gi;
    int err;

    err = unzGetGlobalInfo64(uf, &gi);
    if (err != UNZ_OK) printf("error %d with zipfile in unzGetGlobalInfo \n", err);

    for (i = 0; i < gi.number_entry; i++) {
        if (do_extract_currentfile(uf, &opt_extract_without_path, &opt_overwrite, password) != UNZ_OK) break;

        if ((i + 1) < gi.number_entry) {
            err = unzGoToNextFile(uf);
            if (err != UNZ_OK) {
                printf("error %d with zipfile in unzGoToNextFile\n", err);
                break;
            }
        }
    }

    return 0;
}

int do_extract_onefile(uf, filename, opt_extract_without_path, opt_overwrite, password) unzFile uf;
const char *filename;
int opt_extract_without_path;
int opt_overwrite;
const char *password;
{
    if (unzLocateFile(uf, filename, CASESENSITIVITY) != UNZ_OK) {
        printf("file %s not found in the zipfile\n", filename);
        return 2;
    }

    if (do_extract_currentfile(uf, &opt_extract_without_path, &opt_overwrite, password) == UNZ_OK)
        return 0;
    else
        return 1;
}

///////////////////////////////////////////////////////////////////////////////
/// Provide a function interface for unzip.
///
///\param fmuFilNam The name of fmu file
///\param tmpPat Temporary path of the output file
///\return 0 if no error occurred
///////////////////////////////////////////////////////////////////////////////
int unpackmz(const char *filNam, char *tmpPat)
{
    const char *zipfilename = filNam;
    const char *password = NULL;
    char filename_try[MAXFILENAME + 16] = "";
    int ret_value = 0;
    int opt_do_extract_withoutpath = 0;
    int opt_overwrite = 1;
    const char *dirname = tmpPat;
    char *cmd;
    char cwd[1024];
    unzFile uf = NULL;

    printDebug("Start Minizip\n");
    if (zipfilename) {

#ifdef USEWIN32IOAPI
        zlib_filefunc64_def ffunc;
#endif

        strncpy(filename_try, zipfilename, MAXFILENAME - 1);
        /* strncpy doesnt append the trailing NULL, of the string is too long. */
        filename_try[MAXFILENAME] = '\0';

#ifdef USEWIN32IOAPI
        fill_win32_filefunc64A(&ffunc);
        uf = unzOpen2_64(zipfilename, &ffunc);
#else
        uf = unzOpen64(zipfilename);
#endif
        if (uf == NULL) {
            strcat(filename_try, ".zip");
#ifdef USEWIN32IOAPI
            uf = unzOpen2_64(filename_try, &ffunc);
#else
            uf = unzOpen64(filename_try);
#endif
        }
    } else {
        printf("Null zipfilename pointer, aborting\n");
        return 1;
    }

    if (!uf) {
        printf("Cannot open %s or %s.zip\n", zipfilename, zipfilename);
        return 1;
    }

    printfDebug("%s opened\n", filename_try);

    cmd = calloc(strlen(dirname) + 9, sizeof(char));
    if (cmd == NULL) {
        printfError("Fail to allocate memory for cmd.\n", tmpPat);
        return -1;
    }

#ifdef _WIN32
    if (_access(dirname, 0) == -1) {
        sprintf(cmd, "mkdir %s", dirname); // Command in windows
#else
    if (access(dirname, F_OK) == -1) {
        sprintf(cmd, "mkdir -p %s", dirname); // Command in linux
#endif

        printfDebug("Generated cmd: \"%s\".\n", cmd);
        if (system(cmd) != 0) {
            printError("Fail to creat temporary folders");
            return -1;
        }
    }

    if (getcwd(cwd, sizeof(cwd)) == NULL) fprintf(stdout, "Cannot get current working dir: %s\n", cwd);

#ifdef _WIN32
    _chdir(dirname); // Command in windows
#else
    if (chdir(dirname) < 0) return -1;        // Command in linux
#endif

    ret_value = do_extract(uf, opt_do_extract_withoutpath, opt_overwrite, password); // Extract all files

    unzClose(uf);

#ifdef _WIN32
    _chdir(cwd); // Command in windows
#else
    if (chdir(cwd) < 0) return -1;            // Command in linux
#endif

    return ret_value;
}
