#!/usr/bin/perl
# a script to test the w language
# written by Tyler Durden, based on script by Hyunyoung Lee

$tested = 0;
$succeeded = 0;
$failed = 0;
$intentional_error = 0;

@test_list = (
    [`./w factorial-example.w`, "result is 120\n"],
    [`./w empty-example.w`, "Testing...\n"],
    [`./w fibonacci.w`, "result of fibonacci 15 is 987"],
    [`./w test3.w`, "True"],
    [`./w test4.w`, "9876"],
    [`./w test5.w`, "x is greater than 3\n"],
    [`./w test6.w`, "Or operator works!"],
    [`./w test7.w`, "Logical operators work!"],
    [`./w test8.w`, "True"],
    [`./w test9.w`, "Total is 36"],
    # [`./w `, ""],
    # [`./w `, ""],
    # [`./w `, ""],
    # [`./w `, ""],
    # [`./w `, ""],
);

for (my $i=0; $i < ( scalar @test_list ); $i++)
{
    $tested += 1;
    if($test_list[$i][0] eq $test_list[$i][1])
    {
        $succeeded += 1; 
    }
    else
    {
        $failed += 1; 
    }
}

print "$tested tested\n";
print "$succeeded + $intentional_error passed and $failed failed";
