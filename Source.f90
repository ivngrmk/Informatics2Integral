program Integral
    use dflib
    !ќбъ€вление переменных
    real, parameter :: pi = 3.1415926535
    integer N
    !*****************************************************************************************!
    !read(*,*) N
    !write(*,*) IntegralError(2.0)
    call GraphicWindow()
    call GraphicAxes()
    !*****************************************************************************************!
    contains
    real function f(xt)
        real xt
        f = sin(xt)
    end function f  
    !----------------------------------------------------------------------------------------!
    real function ComputeIntegral(Nt) !ќтрезок от 0 до pi
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
    !---------------------------------------------------------------------------------------!
    subroutine GraphicWindow() !—оздаЄт окно дл€ вывода графики. ‘он - белый, граница - чЄрна€, само окно - белое.
        logical(2) bool2
        integer Pxl, Pxr, Pyu, Pyd
        Pxl = 100; Pyl = 50; Pxr = 900; Pyr = 650
        bool2 = SetBkColor(15); call ClearScreen(0) !ќкраска всего экрана в белый.
        bool2 = SetColor(0); bool2 = Rectangle($GBORDER,Pxl-1, Pyl-1, Pxr+1, Pyr+1) !—оздание границ окна.
        call SetViewPort(Pxl, Pyl, Pxr, Pyr) !—оздание рабочей области.
        bool2 = SetBkColor(15); call ClearScreen(1) !ќкраска рабочей области под график в белый. 1 - —сылка на эту рабочую область.
    end subroutine GraphicWindow
    !---------------------------------------------------------------------------------------!
    subroutine GraphicAxes()
        real xl, yl, xr, yr, scale_width
        real x, y
        type (wxycoord) wxy
        xl = -0.1; yl = -6.0; xr = 7.0; yr = 0.1; scale_width = 0.05 !ќб€зательно должны содержать начало координат
        bool2 = SetWindow(.TRUE., DBLE(xl), DBLE(yl), DBLE(xr), DBLE(yr))
        x = xl
        do while (ceiling(x) <= floor(xr))
            call MoveTo_w(DBLE(ceiling(x)), DBLE(0.0 - scale_width), wxy)
            bool2 = LineTo_w(DBLE(ceiling(x)), DBLE(0.0 + scale_width))
            x = x + 1
        end do
        y = yl
        do while (ceiling(y) <= floor(yr))
            call MoveTo_w(DBLE(0.0 - scale_width), DBLE(ceiling(y)), wxy)
            bool2 = LineTo_w(DBLE(0.0 + scale_width), DBLE(ceiling(y)))
            y = y + 1
        end do
        bool2 = SetColor(4) !–исуем сами оси
        call MoveTo_w(DBLE(xl), DBLE(0.0), wxy)
        bool2 = LineTo_w(DBLE(xr), DBLE(0.0))
        call MoveTo_w(DBLE(0.0), DBLE(yl), wxy)
        bool2 = LineTo_w(DBLE(0.0), DBLE(yr))
    end subroutine GraphicAxes
end
    