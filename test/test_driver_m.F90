program unittests
    use toast     !< testing library
    use test_linked_list_m
    implicit none

    type(TestSuite) :: suite
    suite = TestSuite(name="FortsRawUnitTests")

    ! add the test cases here
    call suite%append(TestFortsLinkedListInteger(name="test_linked_list_integer"))

    ! Run them and print output
    call suite%runall()
    call printsummary(suite)

    ! write to JSON
    call jsonwritetofile(suite, "fortstestresults.json")

    ! check the result
    call suite%checkfailure()

end program unittests