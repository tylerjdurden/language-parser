var arg    = 15;
var result = 0;

var prev0          = 0;
var prev1          = 1;
var currentCounter = 0;
var currentValue   = 0;

while (currentCounter < arg)
{
    currentCounter = currentCounter + 1;
    currentValue   = prev0 + prev1;
    var temp       = prev0;
    prev0          = prev1;
    prev1          = prev1 + temp;
}

result = currentValue;

print "result of fibonacci ";
print arg;
print " is ";
print result;
