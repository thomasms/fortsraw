!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                               !!
!!                    FortsRaw                   !!
!!                                               !!
!!      Copyright (c) 2019, Thomas Stainer       !!
!!                                               !!
!!            All rights reserved.               !!
!!    Licensed under the 3-clause BSD license.   !!
!!                                               !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!> Module for linked list in fortran
module linked_list_m
    use fork_m
    implicit none
    private

    !> list node type - takes ownership of deallocation of the value pointer in finalize
    type, public :: LinkedListNode
        class(*), pointer               :: value    => null()
        type(LinkedListNode), pointer   :: next     => null()
    contains
        final :: nodefinalize
    end type LinkedListNode

    !> list type - takes ownership of deallocation of the value pointer in finalize
    !! Note - do not copy lists, i.e. list1 = list2, this causes memory issues, always pass by reference
    !! Do not return a list from a function
    !! ToDo: Implement copying of lists, do not deep copy pointers
    !! ToDo: Give user option for taking ownership or not (i.e. is user responsible for deallocation of pointers or not)
    type, public :: LinkedList
    private
        integer(ki4)                  :: size = 0_ki4
        type(LinkedListNode), pointer :: head => null()
        type(LinkedListNode), pointer :: tail => null()
    contains
        procedure :: append
        procedure :: first
        procedure :: last
        procedure :: atindex
        procedure :: reset
        procedure :: length
        procedure :: traverse
        procedure, private :: cleanup
            final :: listfinalize
    end type LinkedList

    !> extends LinkedList but makes use of a cached last node
    !! if using getatindex in an iterative manner
    !! Improves performance for large lists
    type, extends(LinkedList), public :: CachedLinkedList
    private
        integer(ki4)                  :: cachedlastindex = 0_ki4
        type(LinkedListNode), pointer :: cachedlastnode => null()
    contains
        procedure :: cachedaccess
        procedure :: reset          => reset_cached
            final :: cachedlistfinalize
    end type CachedLinkedList

contains

    ! Clean up node - The value is deallocated here
    subroutine nodefinalize(this)
        type(LinkedListNode), intent(inout) :: this

        if(associated(this%value))then
            deallocate(this%value)
            nullify(this%value)
            nullify(this%next)
        end if
    end subroutine nodefinalize

    !> Add a value to the list at the tail
    subroutine append(this, value)
        class(LinkedList), intent(inout) :: this
        class(*), intent(in), pointer    :: value

        type(LinkedListNode), pointer :: node_ptr, next_ptr, current_ptr

        ! Create a new node and set the value
        allocate(node_ptr)
        node_ptr%value => value
        node_ptr%next => null()
        this%size = this%size + 1_ki4

        if(.not. associated(this%head))then
            this%head => node_ptr
            this%tail => node_ptr
        else
            this%tail%next => node_ptr
            this%tail      => node_ptr
        end if

    end subroutine append

    !> Traverse the list
    subroutine traverse(this, iterator_func)
        class(LinkedList), intent(inout) :: this
        interface
            subroutine iterator_func(node)
                import LinkedListNode
                type(LinkedListNode), pointer, intent(inout)  :: node
            end subroutine iterator_func
        end interface

        type(LinkedListNode), pointer :: current_ptr, temp_ptr

        current_ptr => this%head
        do while(associated(current_ptr))
            nullify(temp_ptr)
            temp_ptr => current_ptr%next
            call iterator_func(current_ptr)
            current_ptr => temp_ptr
        end do

    end subroutine traverse

    !> Reset the list and cleanup
    subroutine reset(this)
        class(LinkedList), intent(inout) :: this

        call this%cleanup()

    end subroutine reset

    !> Get the size of the list
    pure function length(this) result(size)
        class(LinkedList), intent(in) :: this
        integer(ki4) :: size

        size = this%size

    end function length

    ! Get the first node
    function first(this) result(firstnode)
        class(LinkedList), intent(in) :: this
        type(LinkedListNode), pointer :: firstnode

        firstnode => this%head

    end function first

    ! Get the last node
    function last(this) result(lastnode)
        class(LinkedList), intent(in) :: this
        type(LinkedListNode), pointer :: lastnode

        lastnode => this%tail

    end function last

    ! Get the node at index
    ! must be between 1 and length()
    function atindex(this, index) result(indexnode)
        class(LinkedList), intent(in) :: this
        integer(ki4), intent(in)      :: index
        type(LinkedListNode), pointer :: indexnode

        integer(ki4) :: i

        nullify(indexnode)
        if(index > 0_ki4 .and. index <= this%size)then
            indexnode => this%head
            do i=1, index-1
                indexnode => indexnode%next
            end do
        end if

    end function atindex

    !> Clean up - deallocation of the nodes in the list
    subroutine listfinalize(this)
        type(LinkedList), intent(inout) :: this

        call this%cleanup()

    end subroutine listfinalize

    !> Clean up - deallocation of the nodes in the list
    subroutine cleanup(this)
        class(LinkedList), intent(inout) :: this

        type(LinkedListNode), pointer    :: current_ptr

        call this%traverse(destroyall)
        nullify(this%head)
        nullify(this%tail)

        contains
            subroutine destroyall(node)
                type(LinkedListNode), pointer, intent(inout)  :: node

                this%head => node%next
                deallocate(node)
                nullify(node)

                this%size = this%size - 1_ki4

            end subroutine destroyall

    end subroutine cleanup

    !> Get the node at index
    ! must be between 1 and length()
    ! It uses the cached index if was set
    ! and then sets the cached node after access
    ! for subsequent calls 
    function cachedaccess(this, index) result(indexnode)
        class(CachedLinkedList), intent(inout) :: this
        integer(ki4), intent(in)               :: index

        type(LinkedListNode), pointer          :: indexnode

        integer(ki4) :: i
        integer(ki4) :: startindx
        
        nullify(indexnode)
        if(index > 0_ki4 .and. index <= this%size)then
            ! if last access was cached then use that for speed if we are after it
            if(this%cachedlastindex > 0_ki4 .and. index >= this%cachedlastindex)then
                indexnode => this%cachedlastnode
                startindx =  this%cachedlastindex
            ! else start at head
            else
                indexnode => this%head
                startindx =  1_ki4
            end if

            do i=startindx, index-1
                indexnode => indexnode%next
            end do
            this%cachedlastindex =  index
            this%cachedlastnode  => indexnode
        end if

    end function cachedaccess

    !> Reset the list and cleanup
    subroutine reset_cached(this)
        class(CachedLinkedList), intent(inout) :: this

        call this%cleanup()
        nullify(this%cachedlastnode)
        this%cachedlastindex = 0_ki4

    end subroutine reset_cached

    !> Clean up - deallocation of the nodes in the list
    subroutine cachedlistfinalize(this)
        type(CachedLinkedList), intent(inout) :: this

        call this%cleanup()

    end subroutine cachedlistfinalize

end module linked_list_m
