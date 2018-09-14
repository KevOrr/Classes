# Project 1

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

Both of these race conditions can be seen with the output from one execution
trace:

    From child 1: counter = 99457
    Child 1 with pid 15698 has just exited
    From child 2: counter = 209756
    Child 2 with pid 15699 has just exited
    From child 3: counter = 243777
    Child 3 with pid 15700 has just exited
    From child 4: counter = 528591
    Child 4 with pid 15701 has just exited
    End of simulation

Obviously the result is less than expected (528591 instead of the expected
11000000), which shows that our program exhibits the first race condition. But
we can also see that even when child 1 exits, the value is less than expected,
which shows that our program exhibits the second race condition.
