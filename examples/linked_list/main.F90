program ll
    use fortsraw
    implicit none

    type(LinkedList) :: list
    class(*), pointer :: general_pointer

    write(*, "(A)") "Appending integer value..."
    allocate(general_pointer, source=4_ki4)
    call list%append(general_pointer)

    write(*, "(A)") "Appending real value..."
    allocate(general_pointer, source=89.3_kr8)
    call list%append(general_pointer)

    write(*, "(A)") "Appending string..."
    allocate(general_pointer, source="A string")
    call list%append(general_pointer)

    write(*, "(A, I2.1)") "Size: ", list%length()

    call list%traverse(printvalues)

    call list%reset()
    write(*, "(A, I2.1)") "Size after reset: ", list%length()

    contains

        subroutine printvalues(node)
            type(LinkedListNode), pointer, intent(inout)  :: node

            select type(p => node%value)
                type is(integer(ki4))
                    write(*, "(A, I2)") "Got integer =", p
                type is(real(kr8))
                    write(*, "(A, F5.1)") "Got real =", p
                type is(character(*))
                    write(*, "(A, A)") "Got string = ", p
            class default
                write(*, "(A)") "ERROR!"
            end select

        end subroutine printvalues

end program ll