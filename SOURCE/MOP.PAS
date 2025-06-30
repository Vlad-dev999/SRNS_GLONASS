               (********         MOP.pas        *********)
type
    float = real;
    ar_20 = array[1..20] of float;
    ar_20_20 = array[1..20] of ar_20;
    ar_20_20_ptr = ^ar_20_20;
    ar_6 = array[1..6] of float;
    ar_6_6 = array[1..6] of ar_6;

var a, b, c: ar_6_6;
       i, j: byte;

procedure MMult_mnl( var A, B, C: ar_20_20; m, n, l: byte );
var i, j, k: byte;
          s: float;
begin
   for k:=1 to m do for j:=1 to l do
      begin
         s:=0.0;
         for i:=1 to n do s:=s+A[k][i]*B[i][j];
         C[k][j]:=s
      end
end;

procedure MMult_mn_1( var A: ar_20_20; var B, C: ar_20; m, n: byte );
var i, k: byte;
       s: float;
begin
   for k:=1 to m do
      begin
         s:=0.0;
         for i:=1 to n do s:=s+A[k][i]*B[i];
         C[k]:=s
      end
end;

procedure MAdd_mn( A, B, C: ar_20_20_ptr; m, n: byte );
var i, j: byte;
begin
   for i:=1 to m do for j:=1 to n do C^[i][j]:=A^[i][j]+B^[i][j]
end;

procedure MTransp( var A: ar_20_20; n: byte );
var  s: float;
  i, j: byte;
begin
   for i:=1 to n do for j:=i+1 to n do if i <> j then
      begin
         s:=A[i][j];         A[i][j]:=A[j][i];        A[j][i]:=s
      end
end;

function MDet_nn( var A: ar_20_20; n: byte ): float;
var
   i, j, k, c, b: byte;
   g, z, d, e, s: float;
begin
   g:=0.0;      z:=1.0;    d:=1.0;
   for k:=1 to n do
      begin
         e:=0.0;
         for i:=k to n do
            for j:=k to n do if Abs( e ) >= Abs( A[i][j] ) then else
               begin
                  e:=A[i][j];        b:=i;       c:=j
               end;
         if k = b then else
            begin
               for j:=k to n do
                  begin
                     s:=A[k][j];    A[k][j]:=A[b][j];     A[b][j]:=s
                  end;
               z:=-z
            end;
         if k = c then else
            begin
               for i:=k to n do
                  begin
                     s:=A[i][k];   A[i][k]:=A[i][c];      A[i][c]:=s
                  end;
               z:=-z
            end;
         for i:=k+1 to n do
            begin
               g:=A[i][k]/A[k][k];
               for j:=k to n do A[i][j]:=A[i][j]-g*A[k][j]
            end
      end;
   for i:=1 to n do d:=d*A[i][i];
   d:=d*z;       MDet_nn:=d
end;

procedure MInverse_nn( var A: ar_20_20; n: byte );
var
     i, j, k: integer;
     c, d, r: float;
           b: array[1..20] of integer;
begin
   d:=1;        r:=1;
   for i:=1 to n do
      begin
         j:=i;
         while ( a[j][i]=0 ) and ( j<n ) do j:=j+1;
         if ( a[j][i]=0.0 ) and ( j=n ) then
            begin
               write( 'SINGULAR MATRIX' );      readln;    halt
            end;
         for k:=1 to n do
            begin
               c:=a[i][k];    a[i][k]:=a[j][k];     a[j][k]:=c
            end;
         b[i]:=j;             a[i][i]:=1.0/a[i][i];
         for k:=1 to n do if k <> i then a[i][k]:=a[i][i]*a[i][k];
         for j:=1 to n do if j <> i then
            begin
               c:=-a[j][i];      a[j][i]:=0;
               for k:=1 to n do a[j][k]:=a[j][k]+c*a[i][k]
            end;
         d:=d/a[i][i]
      end;
   for i:=n downto 1 do
      begin
         j:=b[i];
         if j <> i then
            begin
               r:=-r;
               for k:=1 to n do
                  begin
                     c:=a[k][i];    a[k][i]:=a[k][j];    a[k][j]:=c
                  end
            end
      end
end;          (*        End  of  MOP.pas - module       *)

begin
   for i:=1 to 6 do for j:=1 to 6 do
      begin
         A[j][i]:=0.0;     b[j][i]:=0.0;   c[j][i]:=0.0
      end;
   for i:=1 to 6 do
      begin
         a[i][i]:=1.0;     b[i][i]:=2.0
      end;
   MAdd_mn( @A, @B, @C, 6, 6 );
end.
