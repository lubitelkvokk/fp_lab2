lab2
=====

An OTP library

Build
-----

    $ rebar3 compile


Test value:
T1 = lab2:insert(10, 10, lab2:empty()).
T2 = lab2:insert(5, 5, T1).    
T3 = lab2:insert(6, 6, T2).
T4 = lab2:insert(3, 3, T3).
T5 = lab2:insert(4, 4, T4).
T6 = lab2:insert(2, 2, T5).
T7 = lab2:insert(1, 1, T6).


