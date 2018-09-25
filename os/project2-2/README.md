# Project 2

Kevin Orr

## Building

    make

## Running

    ./main

## Observations

Without proper thread synchronization, concurrent processes/threads can exhibit
race conditions. One example of this can be seen by concurrently incrementing a
value in shared memory. If an increment operation is implmented as three
instructions (read, increment, write), then on race condition can occur if two
processes read the value before one of the writes to it. For example, if the
current value is 1, both processes will read a 1 into a register, increment that
register to 2, and write 2 back out to memory. This effectively "loses" an
increment.

Even worse, if a process yields execution in the middle of a
read-increment-write cycle, and regains execution in the future, it can undo all
of the increments that happened in between, when it overwrites the current value
with an older version.

In project 2, we introduce a mutex (implemented as a semaphore initialized to
1). This allows us to mutually exclude threads/processes from incrementing the
same shared value simultaneously.

Compare project 1's output:

    From child 1: counter = 99457
    Child 1 with pid 15698 has just exited
    From child 2: counter = 209756
    Child 2 with pid 15699 has just exited
    From child 3: counter = 243777
    Child 3 with pid 15700 has just exited
    From child 4: counter = 528591
    Child 4 with pid 15701 has just exited
    End of simulation
    
with project 2's:

    From child 1: counter = 100000
    Child 1 with pid 11615 has just exited
    From child 2: counter = 300000
    Child 2 with pid 11616 has just exited
    From child 3: counter = 600000
    Child 3 with pid 11617 has just exited
    From child 4: counter = 1100000
    Child 4 with pid 11618 has just exited
    End of simulation

This shows that the mutual exclusion correctly synchronizes processes so they
can safely increment a shared counter.
