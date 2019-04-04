# -*- makefile -*-

abc : [X] GTK COMMON abc abc-icon|no-icon

abc : [G] WINDOWS COMMON abc abc.res|noicon.res

abcsolver :    [U] abc[STANDALONE_SOLVER] STANDALONE
abcsolver :    [C] abc[STANDALONE_SOLVER] STANDALONE

ALL += abc[COMBINED]

!begin am gtk
GAMES += abc
!end

!begin >list.c
    A(abc) \
!end

!begin >gamedesc.txt
abc:abc.exe:ABC:Letter-placing puzzle:Place A, B or C (one per row, column) depending on letters on the board's edge.
!end
