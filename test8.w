var x = 5;
var result = 0;

{
    var x = 6;
    result = result + x;
}

result = result + x;
print (result == 11);
