c############### layout.h ##############################################
c last modified by MK, date 19.07.2016
c use -ffixed-line-length-none !
c use these functions to cast n-dimensional arrays into one
c dimensional arrays. Fortran fails to initialize arrays with 
c dimensionality higher than 7!

#ifndef LAYOUT_H
#define LAYOUT_H

#define layout1(n1,N1) (n1)
#define layout2(n1,n2,N1,N2) (n1+N1*(n2-1))
#define layout3(n1,n2,n3,N1,N2,N3) (n1+N1*(n2-1+N2*(n3-1)))
#define layout4(n1,n2,n3,n4,N1,N2,N3,N4) (n1+N1*(n2-1+N2*(n3-1+N3*(n4-1))))
#define layout5(n1,n2,n3,n4,n5,N1,N2,N3,N4,N5) (n1+N1*(n2-1+N2*(n3-1+N3*(n4-1+N4*(n5-1)))))
#define layout6(n1,n2,n3,n4,n5,n6,N1,N2,N3,N4,N5,N6) (n1+N1*(n2-1+N2*(n3-1+N3*(n4-1+N4*(n5-1+N5*(n6-1))))))
#define layout7(n1,n2,n3,n4,n5,n6,n7,N1,N2,N3,N4,N5,N6,N7) (n1+N1*(n2-1+N2*(n3-1+N3*(n4-1+N4*(n5-1+N5*(n6-1+N6*(n7-1)))))))
#define layout8(n1,n2,n3,n4,n5,n6,n7,n8,N1,N2,N3,N4,N5,N6,N7,N8) (n1+N1*(n2-1+N2*(n3-1+N3*(n4-1+N4*(n5-1+N5*(n6-1+N6*(n7-1+N7*(n8-1))))))))
#define layout9(n1,n2,n3,n4,n5,n6,n7,n8,n9,N1,N2,N3,N4,N5,N6,N7,N8,N9) (n1+N1*(n2-1+N2*(n3-1+N3*(n4-1+N4*(n5-1+N5*(n6-1+N6*(n7-1+N7*(n8-1+N8*(n9-1)))))))))

c ... you get the idea

#endif
c############### end layout.h ##########################################