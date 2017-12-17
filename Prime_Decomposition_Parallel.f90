program Primes

    use ISO_FORTRAN_ENV

    implicit none

    integer(int64), dimension(7) :: dados = (/2099726827, 15780709, 1122725370, 15808973, 576460741, 12878611, 12757923/)
    integer(int64), dimension(100) :: outprimes
    integer(int64) :: largest_factor = 0, largest = 0, minim = 0, val = 0
    integer(int32) :: count = 0, OMP_GET_THREAD_NUM

    call omp_set_num_threads(4);
    !$omp parallel do private(val,outprimes,count) shared(dados,largest_factor,largest)
    do val = 1, 7
        outprimes = 0
        call find_factors(dados(val), outprimes, count)
        minim = minval(outprimes(1:count))
        if (minim > largest_factor) then
            largest_factor = minim
            largest = dados(val)
        end if
        write(*, fmt = '(A7,i0,A2,i12,100i12)') 'Thread ', OMP_GET_THREAD_NUM(), ': ', dados(val), outprimes(1:count)
    end do
    !$omp end parallel do

    write(*, fmt = '(i0,A26,i0)') largest, ' have the Largest factor: ', largest_factor

    return

contains

    subroutine find_factors(n, d, count)
        integer(int64), intent(in) :: n
        integer(int64), dimension(:), intent(out) :: d
        integer(int32), intent(out) :: count
        integer(int32) :: i
        integer(int64) :: div, next, rest

        i = 1
        div = 2; next = 3; rest = n

        do while (rest /= 1)
            do while (mod(rest, div) == 0)
                d(i) = div
                i = i + 1
                rest = rest / div
            end do
            div = next
            next = next + 2
        end do
        count = i - 1
    end subroutine find_factors

end program Primes
