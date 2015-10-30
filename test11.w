//check if small is a factor of big very inefficiently

var small = 8;
var big = 64;

while (big >= small)
{
    big = big - small;
}

print (big == 0);
