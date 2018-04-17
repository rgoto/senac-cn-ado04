PROGRAM newton
    Implicit None

    CHARACTER(len=32) :: arg
    REAL*8 :: x, aux, result
    INTEGER :: i
    i = 1
    CALL getarg(i, arg) !conseguindo os arqumentos de ARG
    READ (arg, *) x    !transformando string para REAL

    aux = x
    x = x/2

    do 20 i = 1, 15, 1
        result = (x - (((x*x)-aux) / (2*x)))
        x = result
    20 continue

    PRINT *, result
END PROGRAM newton
