program Integral
    !Объявление переменных
    real, parameter :: pi = 3.1415926535
    integer N
    !*****************************************************************************************!
    read(*,*) N
    write(*,*) IntegralError(2.0)
    
    !*****************************************************************************************!
    contains
    real function f(xt)
        real xt
        f = sin(xt)
    end function f  
    !----------------------------------------------------------------------------------------!
    real function ComputeIntegral(Nt) !Отрезок от 0 до pi
        integer Nt
        integer i, N_true, N_all
        real x, y
        N_true = 0; N_all = 0
        do i=1,Nt
            call RANDOM_NUMBER(x)
            x = x * pi
            call RANDOM_NUMBER(y)
            if (f(x) > y) then
                N_true = N_true + 1
            end if
            N_all = N_all + 1
        end do
        ComputeIntegral = real(N_true) / real(N_all) * pi
    end function ComputeIntegral
    !---------------------------------------------------------------------------------------!
    real function IntegralError(I_real)
        real I_real
        real Sum
        integer i
        call RANDOM_SEED()
        Sum = 0
        do i=1,100
            Sum = Sum + abs(ComputeIntegral(N) - I_real)
        end do
        IntegralError = Sum / 100.0
    end function IntegralError
end
    