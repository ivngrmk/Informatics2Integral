program Integral
    use dflib
    !Объявление переменных
    real, parameter :: pi = 3.1415926535
    integer N
    type (wxycoord) wxy
    !*****************************************************************************************!
    call GraphicWindow()
    call GraphicAxes()
    open(1, file= "out.txt")
    call IntegralErrorLogArrayAndGraphic()
    close(1)
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
    real function IntegralError(I_real, N_t)
        real I_real
        real Sum
        integer i, N_t
        call RANDOM_SEED()
        Sum = 0
        do i=1,100
            Sum = Sum + abs(ComputeIntegral(N_t) - I_real)
        end do
        IntegralError = Sum / 100.0
    end function IntegralError
    !---------------------------------------------------------------------------------------!
    subroutine IntegralErrorLogArrayAndGraphic()
    real I_real
        integer i, N_i
        real temp
        real logN_i
        call RANDOM_SEED()
        bool2 = SetColor(4)
        I_real = 2.0 ! Задание релаьного значения вычисляемого интеграла
        call MoveTo_w(DBLE(1), DBLE(-0.5), wxy) !Точка от балды
        do i=0,200
            logN_i = 1 + DBLE((6 - 1)) / 200.0 * i
            x = logN_i
            temp = x * 2.3025851 !Число - натуральный логарифм десяти.
            y = log10(DBLE(IntegralError(I_real, floor(exp(temp)))))
            bool2 = LineTo_w( DBLE(x), DBLE(y) )
            write(1,*)  x, " ", y
        end do
    end subroutine IntegralErrorLogArrayAndGraphic
    !---------------------------------------------------------------------------------------!
    subroutine GraphicWindow() !Создаёт окно для вывода графики. Фон - белый, граница - чёрная, само окно - белое.
        logical(2) bool2
        integer Pxl, Pxr, Pyu, Pyd
        Pxl = 100; Pyl = 50; Pxr = 900; Pyr = 650
        bool2 = SetBkColor(15); call ClearScreen(0) !Окраска всего экрана в белый.
        bool2 = SetColor(0); bool2 = Rectangle($GBORDER,Pxl-1, Pyl-1, Pxr+1, Pyr+1) !Создание границ окна.
        call SetViewPort(Pxl, Pyl, Pxr, Pyr) !Создание рабочей области.
        bool2 = SetBkColor(15); call ClearScreen(1) !Окраска рабочей области под график в белый. 1 - Ссылка на эту рабочую область.
    end subroutine GraphicWindow
    !---------------------------------------------------------------------------------------!
    subroutine GraphicAxes()
        real xl, yl, xr, yr, scale_width
        real x, y
        xl = -0.1; yl = -6.0; xr = 7.0; yr = 0.1; scale_width = 0.05 !Обязательно должны содержать начало координат
        bool2 = SetWindow(.TRUE., DBLE(xl), DBLE(yl), DBLE(xr), DBLE(yr))
        x = xl
        do while (ceiling(x) <= floor(xr)) !Градуировка шкалы абсцисс.
            call MoveTo_w(DBLE(ceiling(x)), DBLE(0.0 - scale_width), wxy)
            bool2 = LineTo_w(DBLE(ceiling(x)), DBLE(0.0 + scale_width))
            x = x + 1
        end do
        y = yl
        do while (ceiling(y) <= floor(yr)) !Градуировка шкалы ординат.
            call MoveTo_w(DBLE(0.0 - scale_width), DBLE(ceiling(y)), wxy)
            bool2 = LineTo_w(DBLE(0.0 + scale_width), DBLE(ceiling(y)))
            y = y + 1
        end do
        bool2 = SetColor(4) !Рисуем сами оси
        call MoveTo_w(DBLE(xl), DBLE(0.0), wxy)
        bool2 = LineTo_w(DBLE(xr), DBLE(0.0))
        call MoveTo_w(DBLE(0.0), DBLE(yl), wxy)
        bool2 = LineTo_w(DBLE(0.0), DBLE(yr))
    end subroutine GraphicAxes
end
    