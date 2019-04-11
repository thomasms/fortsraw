module test_linked_list_m
    use toast
    use fortsraw
    implicit none
    private

    !> Test append
    type, extends(TestCase), public :: TestFortsLinkedListInteger
    contains
        procedure :: test => test_linked_list_integer
    end type TestFortsLinkedListInteger

contains

    ! test integer linked list
    subroutine test_linked_list_integer(this)
        class(TestFortsLinkedListInteger), intent(inout) :: this
        
        type(LinkedList) :: linked_list

        integer(ki4), parameter, dimension(4) :: values = (/2, 14, -5, 0/)
        integer(ki4) :: counter

        call this%assertequal(0_ki4, linked_list%getsize(), &
                & message="Size of linked list should be zero before any items added")
        call linked_list%reset()
        call linked_list%reset()
        call this%assertequal(0_ki4, linked_list%getsize(), &
                & message="Size of linked list should be zero before any items added")

        ! Set the first value
        call allocateandappend(values(1))
        call assertfirstandlastvalue(values(1), values(1))

        ! Set the second value
        call allocateandappend(values(2))
        call assertfirstandlastvalue(values(1), values(2))

        ! Set the third value
        call allocateandappend(values(3))
        call assertfirstandlastvalue(values(1), values(3))

        ! Set the forth value
        call allocateandappend(values(4))
        call assertfirstandlastvalue(values(1), values(4))

        call this%assertequal(4_ki4, linked_list%getsize(), &
            & message="Size of linked list should be 4 after adding all the items")
        counter = 1_ki4
        call linked_list%traverse(asserteachentry)

        ! assert at index
        call assertatindexvalue(values(1), 1_ki4)
        call assertatindexvalue(values(2), 2_ki4)
        call assertatindexvalue(values(3), 3_ki4)
        call assertatindexvalue(values(4), 4_ki4)

        call linked_list%reset()
        call linked_list%reset()
        call this%assertequal(0, linked_list%getsize(), &
                & message="Size of linked list should be zero after reset")

        ! Set the first value again after reset
        call allocateandappend(3_ki4)
        call assertfirstandlastvalue(3_ki4, 3_ki4)

        contains

            subroutine asserteachentry(node)
                type(LinkedListNode), pointer, intent(inout)  :: node

                select type(p => node%value)
                    type is(integer)
                        call this%assertequal(values(counter), p, &
                            & message="Each value should be set correctly")
                class default
                        call this%assertfalse(.true., & 
                            & message="Pointer type should be of integer type")
                end select

                counter = counter + 1_ki4

            end subroutine asserteachentry

            subroutine allocateandappend(valuetoadd)
                integer(ki4), value, intent(in) :: valuetoadd

                class(*), pointer :: general_pointer
                integer(ki4) :: prev_size, i

                prev_size = linked_list%getsize()

                i = valuetoadd
                allocate(general_pointer, source=i)
                call linked_list%append(general_pointer)
                general_pointer => null()
                call this%assertequal(prev_size + 1_ki4, linked_list%getsize(), &
                    & message="Size of linked list should be increased by 1")

            end subroutine allocateandappend

            subroutine assertfirstandlastvalue(firstvalue, lastvalue)
                integer(ki4), value, intent(in) :: firstvalue, lastvalue

                type(LinkedListNode), pointer :: node_ptr

                node_ptr => linked_list%first()
                select type(p => node_ptr%value)
                    type is(integer)
                        call this%assertequal(firstvalue, p, &
                            & message="First value should be set correctly")
                    class default
                        call this%assertfalse(.true., &
                            & message="Pointer type should be of integer type")
                end select

                node_ptr => linked_list%last()
                select type(p => node_ptr%value)
                    type is(integer)
                        call this%assertequal(lastvalue, p, &
                            & message="Last value should be set correctly")
                    class default
                        call this%assertfalse(.true., &
                            & message="Pointer type should be of integer type")
                end select

            end subroutine assertfirstandlastvalue

            subroutine assertatindexvalue(expectedvalue, index)
                integer(ki4), value, intent(in) :: expectedvalue, index

                type(LinkedListNode), pointer :: node_ptr

                node_ptr => linked_list%atindex(index)
                select type(p => node_ptr%value)
                    type is(integer)
                        call this%assertequal(expectedvalue, p, & 
                            & message="Value at index should be set correctly")
                    class default
                        call this%assertfalse(.true., &
                            & message="Pointer type should be of integer type")
                end select

            end subroutine assertatindexvalue

    end subroutine test_linked_list_integer

end module test_linked_list_m