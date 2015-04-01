#!/usr/bin/env python
from _tConvert import *

def dotest(t):

    print ''
    print 'begin dotest'
    print t.testbool (True);
    print t.testbool (False);
    print t.testint (-1);
    print t.testint (10L);
    print t.testint64 (-123456789013L);
    print t.testint64 (123456789014L);
    print t.testssize (-2);
    print t.testssize (11);
    print t.testfloat (3.14);
    print t.testfloat (12);
    print t.teststring ("this is a string");

    print t.testipos ([2,3,4]);
    print t.testipos (1);
    print t.testipos (NUM.array([2]));
    print t.testipos (NUM.array(3));

    print t.testvecbool ([True,False,False,True])
    print t.testvecint ([1,2,3,4]);
    print t.testvecint ([]);
    print t.testvecint ((-1,-2,-3,-4));
    print t.testvecint (-10);
    print t.testvecint (NUM.array((10,11,12)));
    print t.testvecint (NUM.array(1));
    print t.testveccomplex ([1+2j, -1-3j, -1.5+2.5j]);
    print t.testvecstr (["a1","a2","b1","b2"])
    print t.testvecstr (())
    print t.testvecstr ("sc1")
    print t.teststdvecbool ([False,True])
    print t.teststdvecuint ([1,2,4])
    print t.teststdvecuint (())
    print t.teststdvecuint (10)
    print t.teststdvecvecuint ([[1,2,4]])
    print t.teststdvecvecuint ((()))
    print t.teststdvecvecuint (())
    print t.teststdvecvecuint ([1,2,4])
    print t.teststdvecvecuint (20)

    print t.testvh (True);
    print t.testvh (2);
    print t.testvh (1234567890123L);
    print t.testvh (1.3);
    print t.testvh (10-11j);
    print t.testvh ("str");
    print t.testvh ([True]) + 0;         # add 0 to convert numpy to integer
    print t.testvh ([2,4,6,8,10]);
    print t.testvh ([1.3,4,5,6]);
    print t.testvh ([10-11j,1+2j]);
#    print t.testvh ([]);
    print t.testvh (["str1","str2"]);
    print t.testvh ({"shape":[2,2],"array":["str1","str2","str3","str4"]});
    a  =  t.testvh ({"shape":[2,2],"array":["str1","str2","str3","str4"]});
    print a;
    print t.testvh (a);

    a  =  t.testvh ([10-11j,1+2j]);
    print a.shape;
    print t.testvh (a);

    b  =  NUM.int32([[2,3],[4,5]]);
    print b;
    print t.testvh (b);

    b  =  NUM.int32([1,2,3,4,5,6,7,8,9,10]);
    print b[2:9:2];
    print t.testvh (b[2:9:2]);

    b  =  NUM.array([1,2,3,4,5,6,7,8,9,10.]);
    print b[2:9:2];
    print t.testvh (b[2:9:2]);
    a = b[2:9:2];
    print t.testvh (a);

    print t.testvh(NUM.array([20.+10j]));
    print t.testvh(NUM.array([21.]));
    print t.testvh(NUM.array(21.));

    print '>>>';
    res = t.testvh (NUM.array([]));
    print '<<<';
    print res.shape;
    print '>>>';
    res = t.testvh (NUM.array([[]]));
    print '<<<';
    print res.shape;

    # Test a sequence of ValueHolders
    print t.teststdvecvh([2, 1.3, [True,False]]);

    # On 64-bit machines the output also contains 'dtype=int32'
    # So leave it out.
    a = t.testrecord({"int":1, "int64":123456789012L, "str":"bc", 'vecint':[1,2,3]})
    print '>>>'
    print a
    print '<<<'
    print 'end dotest'
    print ''


def testarrvh(arr):
    print '    testarrvh';
    print t.testvh(arr);
    print t.testvh(arr[0]);
    print t.testvh([arr[0]]);
    print t.testvh([arr[0], arr[1]]);

def testarrb(arr):
    testarrvh(arr);
    print t.testbool(arr[0]);

def testarri(arr):
    testarrvh(arr);
    print t.testint(arr[0]);
    print t.testint64(arr[0]);
    print t.testssize(arr[0]);
    print t.testfloat(arr[0]);
    print t.testcomplex(arr[0]);

def testarrf(arr):
    testarrvh(arr);
    print t.testfloat(arr[0]);
    print t.testcomplex(arr[0]);

def testarrc(arr):
    testarrvh(arr);
    print t.testcomplex(arr[0]);

def testnps():
    testarrb(NUM.array([True,False]));
    testarri(NUM.int8([-6,-7]));
    testarri(NUM.uint8([5,6]));
    testarri(NUM.int16([-16,-17]));
    testarri(NUM.uint16([15,16]));
    testarri(NUM.int32([-26,-27]));
    testarri(NUM.uint32([25,26]));
    testarri(NUM.int64([-36,-37]));
    testarri(NUM.uint64([35,36]));
    testarrf(NUM.float32([-46,-47]));
    testarrf(NUM.float64([45,46]));
    testarrc(NUM.complex64([-56-66j,-57-67j]));
    testarrc(NUM.complex128([-76-86j,-77-87j]));

def testnp():
    # Test byte and sbyte.
    b = NUM.int8([-1,-2]);
    print t.testvh(b);
    b = NUM.uint8([211,212]);
    print t.testvh(b);
    print '>>>';
    res = t.testvh(NUM.array([]));
    print '<<<';
    print res.shape;
    print t.testvh(NUM.array([["abcd","c"],["12","x12"]]));
    testnps();

if __name__ == "__main__":

    import numpy as NUM;
    t = tConvert();
    print "Doing numpy/array test ..."
    testnp();
    # Do other tests.
    dotest(t)
