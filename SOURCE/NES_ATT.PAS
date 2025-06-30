           (*   Attestation Near-Earth Satellite by GLONASS   *)
{$A+}
{$DEFINE Real}
{$IFDEF Real}
uses Crt, Dos, Graph;
type   float = Real;
{$ENDIF}
{$IFDEF Single}
{$N+}
{$E+}
uses Crt, Dos, Graph;
type   float = Single;
{$ENDIF}
{$IFDEF Double}
{$N+}
{$E+}
uses Crt, Dos, Graph;
type   float = Double;
{$ENDIF}
{$IFDEF Extended}
{$N+}
{$E+}
uses Crt, Dos, Graph;
type   float = Extended;
{$ENDIF}
{$V-}

const Size = 700;
    Npoint = 700;
     N_SAT = 30;
        Ti = 9;
    Monthes: array [1..12] of byte=( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );
    SL_Mes = '   Near-Earth Satellite attestation by GLONASS. ' +
                'V-2.0.           Kharkov, 1995.';
        FS = Sizeof( float );
    SLight = 2.997456E+8;          { Скорость света, [м/с] }
   SLight2 = SLight*SLight;        { Квадрат скорости света, [м2/с2] }
       Omz = 0.7292115147*1E-4;    { Угловая частота вращения Земли, [рад/c] }
        Sb = 1E-3;
       H00 = 9.0e+5;               { [m] }
       C20 = -1082.637e-6;
       C30 = 2.54e-6;
       C40 = 1.619e-6;
       C22 = 1.54e-6;
       D22 = -0.88e-6;
        mz = 0.3986012*1E+15;   { Гравитационный потенциал Земли, [м3/с2] }
       ez2 = 6.6934216*1E-3;    { Квадрат Земного эксцентриситета }
        az = 6378136.0;         { Экваториальный радиус Земли, [м] }

type Point_Str = ^String;
     StateType = ( Passive, Active, Select ) ;
        Signum = ( Kepler, Ephemerids, Coord, Other );
         OnOff = ( On, Off );

     Almanah_par = Record
           tOm: float;
             T: float;
             e: float;
             i: float;
            Om: float;
           arg: float;
            Tv: float
         end;

     SatInfo = Record
           Npoint: word;    { Количество рабочих точек (отсчетов) }
           Nspace: word     { Количество отсчетов до рабочего сеанса }
        end;


     Sat = Record
           Tbeg, Tend: float; {  Начало и конец видимости  }
           T_Vis: float;      {  Продолжительность видимости, [с]  }
           N_Vis: boolean     {  Признак видимости  }
        end;

     Date = Record
           day, month, year: byte
        end;

     Efm_P = record
           Vx, Vy, Vz: float;     { Проекции вектора скорости, [м/с] }
              x, y, z: float;     { Координаты спутника, [м] }
         end;

     Kepler_P = record
           a: float;        { Большая полуось орбиты, [м] }
           e: float;        { Эксцентриситет орбиты }
           i: float;        { Наклонение орбиты, [рад] }
          Om: float;        { Долгота восходящего узла, [рад] }
           w: float;        { Аргумент преигея, [рад] }
          tp: float;        { Время пролета перигея, [сек] }
       end;

     Coord_Deg = record
           Deg, Min: integer; { Градусы и минуты }
                Sec: float;   { Секунды }
        end;

     Point_Country = record
           B, L: Coord_deg;   { Широта и долгота }
              H: float;       { Высота }
        end;

     Twix = record
           a1, a2: float
        end;

         ar_Sat = array[1..N_SAT] of Sat;
      Efm_P_Ptr = ^Efm_P;
       Ptr_Delt = array[1..Npoint] of ^Efm_P;
         Sat_ar = array[1..Size] of Efm_P_Ptr;
     Sat_ar_Ptr = ^Sat_ar;


               (********         MOP.pas        *********)
type
    ar_20        = array[1..20] of float;
    ar_20_20     = array[1..20] of ar_20;
    ar_20_20_ptr = ^ar_20_20;
    ar_20_ptr    = ^ar_20;
    ar_6         = array[1..6] of float;
    ar_6_6       = array[1..6] of ar_6;
    ar_400       = array[1..400] of float;
    ar_400_ptr   = ^ar_400;
    ar_20_6      = array[1..20] of ar_6;
    ar_6_20      = array[1..6] of ar_20;

procedure MNul( A: ar_400_ptr; n: word );
var i: word;
begin
   for i:=1 to n do A^[i]:=0.0
end;

procedure MMult_mnl( A, B, C: ar_400_ptr; m, n, l, ms, ns, ls: byte );
var i, j, k: byte;
          s: float;
begin
   for k:=1 to ms do for j:=1 to ls do
      begin
         s:=0.0;
         for i:=1 to ns do
            s:=s+A^[(k-1)*n+i]*B^[(i-1)*l+j];
         C^[(k-1)*l+j]:=s
      end
end;

procedure MAdd_mn( A, B, C: ar_400_ptr; m, n, ms, ns: byte );
var i, j: byte;
begin
   for i:=1 to ms do for j:=1 to ns do
         C^[i+(j-1)*n]:=A^[i+(j-1)*n]+B^[i+(j-1)*n]
end;

procedure MTransp_mn( A, B: ar_400_ptr; m, n, ms, ns: byte );
var i, j: byte;
begin
   for i:=1 to ns do for j:=1 to ms do
         B^[(i-1)*m+j]:=A^[(j-1)*n+i]
end;

procedure MInverse_nn( A: ar_400_ptr; n, ns: byte );
var
     i, j, k: integer;
     c, d, r: float;
           b: array[1..20] of integer;

begin
   d:=1;        r:=1;
   for i:=1 to ns do
      begin
         j:=i;
         while ( a^[(j-1)*n+i]=0 ) and ( j<n ) do j:=j+1;
         if ( a^[(j-1)*n+i]=0.0 ) and ( j=n ) then
            begin
               write( 'SINGULAR MATRIX' );      readln;    halt
            end;
         for k:=1 to ns do
            begin
               c:=a^[(i-1)*n+k];    a^[(i-1)*n+k]:=a^[(j-1)*n+k];
               a^[(j-1)*n+k]:=c
            end;
         b[i]:=j;             a^[(i-1)*n+i]:=1.0/a^[(i-1)*n+i];
         for k:=1 to ns do if k <> i then a^[(i-1)*n+k]:=a^[(i-1)*n+i]*a^[(i-1)*n+k];
         for j:=1 to ns do if j <> i then
            begin
               c:=-a^[(j-1)*n+i];      a^[(j-1)*n+i]:=0;
               for k:=1 to n do a^[(j-1)*n+k]:=a^[(j-1)*n+k]+c*a^[(i-1)*n+k]
            end;
         d:=d/a^[(i-1)*n+i]
      end;
   for i:=ns downto 1 do
      begin
         j:=b[i];
         if j <> i then
            begin
               r:=-r;
               for k:=1 to ns do
                  begin
                     c:=a^[(k-1)*n+i];    a^[(k-1)*n+i]:=a^[(k-1)*n+j];
                     a^[(k-1)*n+j]:=c
                  end
            end
      end
end;          (*        End  of  MOP.pas - module       *)



var
   X0, Y0, Z0, L0, B0, H0, tk1, tk2, Step1, Step: float;
       Time_beg, Time, tetX, tetY, tetZ, RK_Step: float;
                         betmax, bet, ttt1, ttt2: float;
   Bc, Tc, x, y, NChoise, ik, hh, mm, ss, N_Work: byte;
          TimeSt, DateSt, Time_Int, Time1, Time2: String[10];
                              InpF1, InpF2, Path: String[79];
     Dir: DirStr;       Name: NameStr;       Ext: ExtStr;
      Pa: PathStr;        st: string;         Na: string[35];
                                            Sstr: String;
                                   Name_Usr, Mes: String[20];
                                          Ar_Str: array[0..20] of Point_Str;
                             Adaptation, Success: Boolean;
                                     Data1, Data: Date;
                                             OKA: Sat_ar;
                                         GLONASS: array[1..N_SAT] of Sat_ar_Ptr;
                                          Nks_ar: array[1..40] of Twix;
                                            Disp: array[1..40] of float;
                                        Sat_Info: array[1..N_SAT] of SatInfo;
                                  Work_Sat, Nsat: array[1..N_SAT] of word;
                                           Plase: Point_Country;
                                  Hk, AAtr, Cktr: ar_6_20;
                                              Ck: ar_20_6;
                                         AA, Nks: ar_20_20;
         Aa6, TET, Ak, Eks, Eks_pr, ei, Fk, Fktr: ar_6_6;
Disp_Lk, Delt_Lk, Noise, Lk_old, Lk_pro, Lk, Lk0: Efm_P;
                                 Nu, dYk, Yk, Rk: ar_20;
(*                              Disp_Lk, delt_Lk: Ptr_Delt;   *)
                                       f, f1, f2: Text;
                               m, k, l, j, i, cd: integer;
                                              ch: char;
                                               N: word;

procedure Kur( Signum: OnOff );
var Regs: Registers;
begin
   with Regs do
      begin
         AH:=2;     DL:=0;   BH:=0;
         Case Signum of
               Off:  DH:=25;
                On:  DH:=0
            end
      end;
   Intr( $10, Regs )
end;

procedure Del_Space( var S: string );
begin
   while ( S[1]=' ' ) or ( S[1]='*' ) or ( S[1]=':' ) do Delete( S, 1, 1 )
end;

procedure Read_Str( var s: string );
var
    Exit: boolean;
    x, y: integer;
      s1: string;
begin
   x:=WhereX;        y:=WhereY;
   TextColor( LightRed + Blink );
   write( s );       GotoXY( x, y );
   Exit:=False;
   TextColor( White );
   repeat
      if keypressed then
         begin
            clreol;      readln( s1 );       Exit:=True
         end
   until Exit;
   if s1='' then else s:=s1;      GotoXY( x, y );      writeln( s );
   TextColor( Tc )
end;

procedure Read_Num( var Num: float );
const ww = 7;
var
    x, y, cd, Wide: integer;
            s1, s2: string[45];
              Exit: boolean;
begin
   x:=WhereX;         y:=WhereY;
   Str( Num:ww:( ww-4 ), s2 );
   TextColor( LightRed + Blink );
   write( s2 );       GotoXY( x, y );
   Exit:=False;
   TextColor( White );
   repeat
      if keypressed then
         begin
            clreol;        readln( s1 );       Exit:=True
         end
   until Exit;
   Wide:=Length( s1 );
   if Wide < 6 then Wide:=6;
   if s1='' then
      begin
         GotoXY( x, y );       writeln( s2 )
      end
            else
      begin
         gotoxy( x, y );       clreol;
         Val( s1, Num, cd );   writeln( s1:Wide )
      end;
   TextColor( Tc )
end;

procedure Win ( x1, y1, x2, y2, Bc, Tc : byte ) ;
begin
   Window( x1, y1, x2, y2 );
   TextBackground( Bc );TextColor( Tc )
end;

procedure Info( s: string ) ;
var x, y : byte ;
   begin
      x:=WhereX;             y:=WhereY;
      Win( 1, 25, 80, 25, 7, Red );                         ClrScr;
      Write( ' '+s );        Win( 1, 2, 80, 25, Bc, Tc );   GotoXY( x, y )
   end;

procedure RK_Int( Y0: Efm_P; var Y: Efm_P; Step: float );
type
        ar_6 = array[1..6] of float;
      ar_4_6 = array[1..4] of ar_6;
var
    x2, y2, sqsq, r, V, Sinfi, Cosfi, Sinla, Cosla, r2, r3:  float;
      sq_Sinfi, P20, P30, P40, P22, dP20, dP30, dP40, dP22:  float;
         az2, Cos2la, Sin2la, dUvdr, dUvdfi, dUvdla, Hight:  float;
                       kol, dUvdx, dUvdy, dUvdz, fn, Ro, h:  float;
                                                   l, i, j:  integer;
                                                         K:  ar_4_6;
                                                     Point:  Efm_P;
function Koeff( i: integer; var K: ar_4_6 ): float;
begin
   Koeff:=( K[1][i]+2.0*( K[2][i]+K[3][i] )+K[4][i] )/6.0
end;

procedure Inc_Point( var Point: Efm_P; var K: ar_6 );
begin
   with Point do
      begin
         Vx:=Y0.Vx+K[1]*kol;            x:=Y0.x+K[4]*kol;
         Vy:=Y0.Vy+K[2]*kol;            y:=Y0.y+K[5]*kol;
         Vz:=Y0.Vz+K[3]*kol;            z:=Y0.z+K[6]*kol
      end
end;

function F1( var Point: Efm_P ): float;
begin
   dUvdx:=Cosfi*Cosla*dUvdr-Sinfi*Sinla*dUvdfi-Sinla*dUvdla;
   with Point do F1:=( Omz*Omz-mz/r3 )*x+2*Omz*Vy-Sb*Ro*V*Vx+dUvdx
end;

function F2( var Point: Efm_P ): float;
begin
   dUvdy:=Cosfi*Sinla*dUvdr-Sinfi*Sinla*dUvdfi+Cosla*dUvdla;
   with Point do F2:=( Omz*Omz-mz/r3 )*y+2*Omz*Vx-Sb*Ro*V*Vy+dUvdy
end;

function F3( var Point: Efm_P ): float;
begin
   dUvdz:=Sinfi*dUvdr+Cosfi*dUvdfi;
   with Point do F3:=-mz/r3*z-Sb*Ro*V*Vz+dUvdz
end;

function F4( var Point: Efm_P ): float;
begin
   with Point do F4:=Vx
end;

function F5( var Point: Efm_P ): float;
begin
   with Point do F5:=Vy
end;

function F6( var Point: Efm_P ): float;
begin
   with Point do F6:=Vz
end;

procedure SDE;
begin
   with Y do
      begin
         x2:=x*x;                  y2:=y*y;           sqsq:=Sqrt( x2+y2 );
         r:=Sqrt( x2+y2+z*z );
         V:=Sqrt( Vx*Vx+Vy*Vy+Vz*Vz );
         Sinfi:=z/r;               Cosfi:=sqsq/r;
         Sinla:=y/sqsq;            Cosla:=x/sqsq;
         r2:=r*r;                  r3:=r*r2
      end;
   sq_Sinfi:=Sqr( Sinfi );
   P20:=0.5*( 3*sq_Sinfi-1.0 );
   P30:=-1.5*Sinfi+2.5*Sinfi*sq_Sinfi;
   P40:=0.125*( 3.0-30*sq_Sinfi+35*Sqr( sq_Sinfi ) );
   P22:=3*Cosfi*Cosfi;
   dP20:=3*Sinfi*Cosfi;
   dP30:=-1.5*Cosfi+7.5*sq_Sinfi*Cosfi;
   dP40:=0.125*( -60*Sinfi*Cosfi+140*Sinfi*sq_Sinfi*Cosfi );
   dP22:=-6*Sinfi*Cosfi;
   az2:=Sqr( az );
   Cos2la:=Sqr( Cosla )-sqr( Sinla );   Sin2la:=2*Sinla*Cosla;
   dUvdr:=-mz/r2/r2*az2*( 3*C20*P20+4*C30*P30*az/r+5*C40*P40*az2/r2+
              3*P22*( C22*Cos2la+D22*Sin2la ) );
   dUvdfi:=mz/r2/r2*az2*Cosfi*( C20*dP20+dP30*C30*az/r+C40*dP40*az2/r2+
              dP22*( C22*Cos2la+D22*Sin2la ) );
   dUvdla:=mz/r2/r2*az2*6*Cosfi*( D22*Cos2la-C22*Sin2la );
   Hight:=r-az*Sqrt( 1.0-ez2 )/Sqrt( 1.0-Cosfi*ez2 );
   Ro:=6.367e-12*Exp( -0.954e-5*( Hight-H00 ) )
end;

begin
   h:=Step;        Y:=Y0;
   for j:=1 to 4 do for i:=1 to 6 do k[j][i]:=0;
   for j:=1 to 4 do
      begin
         SDE;
         case j of
               1: begin  l:=j;     kol:=0.5   end;
               2: begin  l:=j-1;   kol:=0.5   end;
               3: begin  l:=j-1;   kol:=1.0   end;
               4: begin  l:=j-1;   kol:=0.0   end
            end;
         for i:=1 to 6 do
            begin
               case i of
                     1: fn:=F1( Y );
                     2: fn:=F2( Y );
                     3: fn:=F3( Y );
                     4: fn:=F4( Y );
                     5: fn:=F5( Y );
                     6: fn:=F6( Y )
                  end;
               K[j][i]:=h*fn
            end;
         case j of
               1, 2, 3: Inc_Point( Y, K[j] )
            end;
      end;
   with Y0 do
      begin
         Vx:=Vx+Koeff( 1, K );
         Vy:=Vy+Koeff( 2, K );
         Vz:=Vz+Koeff( 3, K );
         x:=x+Koeff( 4, K );
         y:=y+Koeff( 5, K );
         z:=z+Koeff( 6, K )
      end;
   Y:=Y0
end;     {    RK_Int    }

procedure AvrSound;
const Song: array[1..5] of word = ( 70, 50, 100, 50, 70 );
var j, i: integer;
begin
   for j:=1 to 2 do
      begin
         for i:=1 to 5 do
            begin
               Sound( Song[i]*j*5 );
               if i=3 then delay( 300 ) else delay( 150 )
            end;
         Nosound;
         Delay( 200 )
      end
end;

procedure OpenInpFile ( path: string; var Success: boolean;
                        sign: Signum; InpF: String );
var c: char;
   Na: string[35];
   begin
      Na:=InpF;
      path:=path+Na;         Assign( f, path );
{$I-}
      if Na = InpF1 then Reset( f1 ) else
         if Na = InpF2 then Reset( f2 ) else
            Reset( f );
{$I+}
      if IOResult <> 0 then
         begin
            Success:=False;
            Info( 'Data file ( '+path+' ) not found . . . Press any key' );
            AvrSound;          c:=Readkey
         end
               else begin Success:=True;
                 Info( 'Loading data file ( '+path+' ) . . .' )
                    end
   end;

procedure ReadUserCoord( Sign: Signum );
var j : byte;
    st: string;
    Na: string[35];
begin
   case Sign of
         Kepler: Na:='Kepler.usr';
          Coord: Na:='user.crd';
       end;
   repeat
      ClrScr;               Write( 'Path to data ( '+Na+' ) file : ' );
      Read_Str( path );     OpenInpFile( path, Success, Sign, '' )
   until Success;
   if Sign=Kepler then Readln( f );           j:=0;
   while ( not Eof( f ) ) and ( j < 20 ) do
      begin
         Readln( f, st );
         if ( Length( st ) > 40 ) and ( st[1] <> '{' ) then
            begin
               New( Ar_Str[j] );
               Ar_str[j]^:=st;
               Inc( j )
            end
      end;
   Close( f );      NChoise:=j-1;
   if NChoise=0 then Info( 'Error : '+Na+' is empty . . . Press any key' )
end;

procedure Choise( NChoise: byte; var Str_Choise: string );
var i, kmax, xx, yy, j, k: byte;
               xcur, ycur: byte;
                     Exit: boolean;
                        c: char;
procedure Menu ( nMelm : byte ; State : StateType ) ;
var Bc, Tc : byte ;
begin
   case State of
         Passive : begin Bc:=1;           Tc:=7       end;
         Select  : begin Bc:=Green;       Tc:=Red     end;
         Active  : begin Bc:=7;           Tc:=1       end
      end;
   Win( xx, yy+nMelm-1, xx+kmax+1, yy+nMelm-1, Bc, Tc );    ClrScr;
   Write( ' '+( Ar_Str[nMelm] )^ );
   Win( 1, 2, 80, 25, 1, 7 );
   GotoXY( Xcur, Ycur )
end;

begin    {   Choise   }
   kmax:=1;      Xcur:=WhereX;         Ycur:=WhereY;       i:=1;
   while ( i <= NChoise ) and ( ( Ar_Str[i] )^ <> '' ) do
      begin
         k:=Length( ( Ar_Str[i] )^ );
         if k > 78 then kmax:=78 else
            if k > kmax then kmax:=k;
         Inc( i )
      end;
      j:=1;
   if i > 1 then
      begin
         xx:=40-( kmax div 2 );        yy:=13-( NChoise div 2 );
         GotoXY( xx+1, yy-2 );         Write( ( Ar_Str[0] )^ );
         Exit:=False;        i:=1;
         Menu( i, Active );
         for i:=2 to NChoise do Menu( i, Passive );
         if NChoise=j then Exit:=True;
         repeat
            if KeyPressed then
               begin
                  c:=ReadKey;
                  if c=#0 then
                     begin
                        c:=ReadKey;
                        case Ord( c ) of
                              80: begin
                                     Menu( j, Passive );
                                     if j=Nchoise then j:=1 else Inc( j );
                                     Menu( j, Active )     end;
                              72: begin
                                     Menu( j, Passive );
                                     if j=1 then j:=Nchoise else Dec( j );
                                     Menu( j, Active )     end;
                              81: begin
                                     Menu( j, Passive );   j:=Nchoise;
                                     Menu( j, Active )     end;
                              73: begin
                                     Menu( j, Passive );   j:=1;
                                     Menu( j, Active )     end;
                           end
                     end
                        else  if c=#13 then
                           begin
                              Exit:=True;    Menu( j, Select );  Delay( 200 )
                           end
               end
         until Exit
      end;      {   if i > 1   }
   Str_Choise:=( Ar_Str[j] )^;
   for i:=0 to Nchoise do Dispose( Ar_Str[i] )
end;      {   of  Choise    }

procedure Read_Data_Coord( var Plase: Point_Country; Sign: Signum;
                            var Name: string );
var
    Str_Choise: String;
           Alt: string[5];
        cd, Ab: integer;

begin
   ReadUserCoord( Coord );
   clrscr;                         Name:='';
   Info( 'Choose working user ( keys: Up, Dn-move; ENTER-choose )' );
   Choise( NChoise, Str_Choise );
   Del_Space( Str_Choise );
   while Str_Choise[1]<>' ' do
      begin
         Name:=Name+Str_Choise[1];
         Delete( Str_Choise, 1, 1 )
      end;
   Del_Space( Str_Choise );
   with Plase do
      begin
         if Str_Choise[1]='-' then  begin
                             Ab:=-1; Delete( Str_Choise, 1, 1 )
                                    end  else Ab:=1;
         with L do
            begin
               Val( Copy( Str_Choise, 1, 3 ), Deg, cd );
               Val( Copy( Str_Choise, 5, 2 ), Min, cd );
               Val( Copy( Str_Choise, 8, 2 ), sec, cd );
               Deg:=Deg*Ab;    Min:=Min*Ab;   sec:=sec*Ab;
            end;
         Delete( Str_Choise, 1, 9 );        Del_Space( Str_Choise );
         with B do
            begin
               if Str_Choise[1]='-' then begin
                                      Ab:=-1;   Delete( Str_Choise, 1, 1 )
                                         end   else Ab:=1;
               Val( Copy( Str_Choise, 1, 2 ), Deg, cd );
               Val( Copy( Str_Choise, 4, 2 ), Min, cd );
               Val( Copy( Str_Choise, 7, 2 ), sec, cd );
               Deg:=Deg*Ab;    Min:=Min*Ab;   sec:=sec*Ab
            end;
         Delete( Str_Choise, 1, 8 );        Del_Space( Str_Choise );
         if Str_Choise[1]='-' then begin
                                     Ab:=-1;  Delete( Str_Choise, 1, 1 )
                                   end   else Ab:=1;
         Alt:='';
         while Str_Choise[1]<>' ' do
            begin
               Alt:=Alt+Str_Choise[1];
               Delete( Str_Choise, 1, 1 )
            end;
         Val( Alt, H, cd );      H:=H*Ab
      end;
end;         {    Read_Data_Coord    }


procedure AvrExit ;
   begin
      AvrSound;    TextBackground( 0 );    ClrScr;    Halt( 1 )
   end;

function Normal: float;
var sum: float;
      i: byte;
begin
   sum:=0.0;     for i:=1 to 12 do sum:=sum+Random;     sum:=sum-6.0;
   Normal:=sum
end;

procedure Calk_Ck( var Ck: ar_20_6 );
var
   dzkz, Dkz, dVxkz, dVykz, dVzkz, Dkj2, Dkz2, dxkz, dykz:  float;
       Vk, V0, dVxkj, dVykj, dVzkj, dxkj, dykj, dzkj, Dkj:  float;
                                                    j, kc:  byte;
                                                       Np:  word;

begin
   for j:=1 to 20 do for kc:=1 to 6 do Ck[j][kc]:=0.0;
   Np:=i-Sat_Info[Work_Sat[1]].Nspace;
   dxkz:=GLONASS[Work_Sat[1]]^[Np]^.x-Lk_pro.x;
   dykz:=GLONASS[Work_Sat[1]]^[Np]^.y-Lk_pro.y;
   dzkz:=GLONASS[Work_Sat[1]]^[Np]^.z-Lk_pro.z;
   dVxkz:=GLONASS[Work_Sat[1]]^[Np]^.Vx-Lk_pro.Vx;
   dVykz:=GLONASS[Work_Sat[1]]^[Np]^.Vy-Lk_pro.Vy;
   dVzkz:=GLONASS[Work_Sat[1]]^[Np]^.Vz-Lk_pro.Vz;

   V0:=dxkz*dVxkz+dykz*dVykz+dzkz*dVykz;
   Dkz:=Sqrt( Sqr( dxkz )+SQR( dykz )+SQR( dzkz ) );
   Dkz2:=Dkz*Dkz;
   for j:=1 to N_Work-1 do
      begin
         Np:=i-Sat_Info[Work_Sat[j+1]].Nspace;
         dxkj:=GLONASS[Work_Sat[j+1]]^[Np]^.x-Lk_pro.x;
         dykj:=GLONASS[Work_Sat[j+1]]^[Np]^.y-Lk_pro.y;
         dzkj:=GLONASS[Work_Sat[j+1]]^[Np]^.z-Lk_pro.z;
         dVxkj:=GLONASS[Work_Sat[j+1]]^[Np]^.Vx-Lk_pro.Vx;
         dVykj:=GLONASS[Work_Sat[j+1]]^[Np]^.Vy-Lk_pro.Vy;
         dVzkj:=GLONASS[Work_Sat[j+1]]^[Np]^.Vz-Lk_pro.Vz;
         Dkj:=Sqrt( dxkj*dxkj+dykj*dykj+dzkj*dzkj );
         Vk:=dxkj*dVxkj+dykj*dVykj+dzkj*dVzkj;
         Dkj2:=Dkj*Dkj;
         for kc:=1 to 3 do Ck[2*j-1][kc]:=0.0;
         Ck[2*j-1][4]:=dxkz/Dkz-dxkj/Dkj;
         Ck[2*j-1][5]:=dykz/Dkz-dykj/Dkj;
         Ck[2*j-1][6]:=dzkz/Dkz-dzkj/Dkj;
         Ck[2*j][1]:=Ck[2*j-1][4];      (*  dxkz/Dkz-dxkj/Dkj;  *)
         Ck[2*j][2]:=Ck[2*j-1][5];      (*  dykz/Dkz-dykj/Dkj;  *)
         Ck[2*j][3]:=Ck[2*j-1][6];      (*  dzkz/Dkz-dzkj/Dkj;  *)
         Ck[2*j][4]:=( dVxkz+V0*dxkz/Dkz2 )/Dkz-( dxkj*Vk/Dkj2+dVxkj )/Dkj;
         Ck[2*j][5]:=( dVykz+V0*dykz/Dkz2 )/Dkz-( dykj*Vk/Dkj2+dVykj )/Dkj;
         Ck[2*j][6]:=( dVzkz+V0*dzkz/Dkz2 )/Dkz-( dzkj*Vk/Dkj2+dVzkj )/Dkj
      end;
end;

procedure Filtering( var Lk: Efm_P; Lk0: Efm_P );
var
  taukj, dxsk0, dysk0, dzsk0, dVxk, dVyk, dVzk, dxkj, dykj:  float;
   Vk, V0, dVxskj, dVyskj, dVzskj, dUvdx, dUvdy, dUvdz, Ro:  float;
   _Dkz, az2, Cos2la, Sin2la, dUvdr, dUvdfi, dUvdla, Hight:  float;
    x2, y2, sqsq, r, V, Sinfi, Cosfi, Sinla, Cosla, r2, r3:  float;
    _dDnnj, _dVxkj, _dVykj, _dVzkj, _ddDkj, _Dnnj, _dtaukj:  float;
    _dzkj, _dDkz, _ddDkz, _Dkj, _dDkj, _dxk0, _dyk0, _dzk0:  float;
     dDnnj, Omz2, dVxkj, dVykj, dVzkj, ddDkj, Dnnj, dtaukj:  float;
      sq_Sinfi, P20, P30, P40, P22, dP20, dP30, dP40, dP22:  float;
       dzkj, dDkz, ddDkz, Dkj, dDkj, dxk0, dyk0, dzk0, Dkz:  float;
       _taukj, _dVxk, _dVyk, _dVzk, _dxkj, _dykj, _Vk, _V0:  float;
               dVxk0, dVyk0, dVzk0, _dVxk0, _dVyk0, _dVzk0:  float;
                                       dVxkz, dVykz, dVzkz:  float;
                                               Ndop, Count:  word;
                                                       dHk:  ar_20;

function F_1( var Point: Efm_P ): float;
begin
   dUvdx:=Cosfi*Cosla*dUvdr-Sinfi*Sinla*dUvdfi-Sinla*dUvdla;
   with Point do F_1:=( Omz*Omz-mz/r3 )*x+2*Omz*Vy-Sb*Ro*V*Vx+dUvdx
end;

function F_2( var Point: Efm_P ): float;
begin
   dUvdy:=Cosfi*Sinla*dUvdr-Sinfi*Sinla*dUvdfi+Cosla*dUvdla;
   with Point do F_2:=( Omz*Omz-mz/r3 )*y+2*Omz*Vx-Sb*Ro*V*Vy+dUvdy
end;

function F_3( var Point: Efm_P ): float;
begin
   dUvdz:=Sinfi*dUvdr+Cosfi*dUvdfi;
   with Point do F_3:=-mz/r3*z-Sb*Ro*V*Vz+dUvdz
end;

procedure SDE( var Lk0: Efm_P );
begin
   with Lk0 do
      begin
         x2:=x*x;                  y2:=y*y;           sqsq:=Sqrt( x2+y2 );
         r:=Sqrt( x2+y2+z*z );
         V:=Sqrt( Vx*Vx+Vy*Vy+Vz*Vz );
         Sinfi:=z/r;               Cosfi:=sqsq/r;
         Sinla:=y/sqsq;            Cosla:=x/sqsq;
         r2:=r*r;                  r3:=r*r2
      end;
   sq_Sinfi:=Sqr( Sinfi );
   P20:=0.5*( 3*sq_Sinfi-1.0 );
   P30:=-1.5*Sinfi+2.5*Sinfi*sq_Sinfi;
   P40:=0.125*( 3.0-30*sq_Sinfi+35*Sqr( sq_Sinfi ) );
   P22:=3*Cosfi*Cosfi;
   dP20:=3*Sinfi*Cosfi;
   dP30:=-1.5*Cosfi+7.5*sq_Sinfi*Cosfi;
   dP40:=0.125*( -60*Sinfi*Cosfi+140*Sinfi*sq_Sinfi*Cosfi );
   dP22:=-6*Sinfi*Cosfi;
   az2:=Sqr( az );
   Cos2la:=Sqr( Cosla )-sqr( Sinla );   Sin2la:=2*Sinla*Cosla;
   dUvdr:=-mz/r2/r2*az2*( 3*C20*P20+4*C30*P30*az/r+5*C40*P40*az2/r2+
              3*P22*( C22*Cos2la+D22*Sin2la ) );
   dUvdfi:=mz/r2/r2*az2*Cosfi*( C20*dP20+dP30*C30*az/r+C40*dP40*az2/r2+
              dP22*( C22*Cos2la+D22*Sin2la ) );
   dUvdla:=mz/r2/r2*az2*6*Cosfi*( D22*Cos2la-C22*Sin2la );
   Hight:=r-az*Sqrt( 1.0-ez2 )/Sqrt( 1.0-Cosfi*ez2 );
   Ro:=6.367e-12*Exp( -0.954e-5*( Hight-H00 ) )
end;

procedure SDE_Sat;
begin
   with GLONASS[Work_Sat[j+1]]^[i-Ndop]^ do
      begin
         r:=Sqrt( x*x+y*y+z*z );
         V:=Sqrt( Vx*Vx+Vy*Vy+Vz*Vz );
         r2:=r*r;                  r3:=r*r2;
         sqsq:=( 1.0-5.0*z*z/r2 )*1.5*C20*mz/r2*Sqr( az )/r3-mz/r3;
      end;
   Omz2:=Sqr( Omz );
end;

begin
   randomize;
   for j:=1 to 20 do dHk[j]:=0.0;
   Ndop:=Sat_Info[Work_Sat[1]].Nspace;
   dxk0:=-( Lk_pro.x-GLONASS[Work_Sat[1]]^[i-Ndop]^.x );
   dyk0:=-( Lk_pro.y-GLONASS[Work_Sat[1]]^[i-Ndop]^.y );
   dzk0:=-( Lk_pro.z-GLONASS[Work_Sat[1]]^[i-Ndop]^.z );
   _dxk0:=-( Lk0.x-GLONASS[Work_Sat[1]]^[i-Ndop]^.x );
   _dyk0:=-( Lk0.y-GLONASS[Work_Sat[1]]^[i-Ndop]^.y );
   _dzk0:=-( Lk0.z-GLONASS[Work_Sat[1]]^[i-Ndop]^.z );
   dVxk0:=GLONASS[Work_Sat[1]]^[i-Ndop]^.Vx-Lk_pro.Vx;
   dVyk0:=GLONASS[Work_Sat[1]]^[i-Ndop]^.Vy-Lk_pro.Vy;
   dVzk0:=GLONASS[Work_Sat[1]]^[i-Ndop]^.Vz-Lk_pro.Vz;
   _dVxk0:=GLONASS[Work_Sat[1]]^[i-Ndop]^.Vx-Lk0.Vx;
   _dVyk0:=GLONASS[Work_Sat[1]]^[i-Ndop]^.Vy-Lk0.Vy;
   _dVzk0:=GLONASS[Work_Sat[1]]^[i-Ndop]^.Vz-Lk0.Vz;

   V0:=dxk0*dVxk0+dyk0*dVyk0+dzk0*dVzk0;
   _V0:=_dxk0*_dVxk0+_dyk0*_dVyk0+_dzk0*_dVzk0;
   Dkz:=Sqrt( dxk0*dxk0+dyk0*dyk0+dzk0*dzk0 );
   _Dkz:=Sqrt( _dxk0*_dxk0+_dyk0*_dyk0+_dzk0*_dzk0 );
   dDkz:=V0/Dkz;             _dDkz:=_V0/_Dkz;
   SDE( LK_pro );
   dVxk:=F_1( Lk_pro );      dVyk:=F_2( Lk_pro );      dVzk:=F_3( Lk_pro );
   SDE( Lk0 );
   _dVxk:=F_1( Lk0 );        _dVyk:=F_2( Lk0 );        _dVzk:=F_3( Lk0 );
   j:=0;
   SDE_Sat;
   with GLONASS[Work_Sat[1]]^[i-Ndop]^ do
      begin
         dVxkz:=x*( sqsq+Omz2 )+2*Omz*Vy;
         dVykz:=y*( sqsq+Omz2 )-2*Omz*Vx;
         dVzkz:=z*sqsq
      end;

   ddDkz:=( Sqr( Lk_pro.Vx-GLONASS[Work_Sat[1]]^[i-Ndop]^.Vx )+
    Sqr( Lk_pro.Vy-GLONASS[Work_Sat[1]]^[i-Ndop]^.Vy )+
    Sqr( Lk_pro.Vz-GLONASS[Work_Sat[1]]^[i-Ndop]^.Vz )+(dVxkz-dVxk)*dxk0+
              (dVykz-dVyk)*dyk0+(dVzkz-dVzk)*dzk0-Sqr( dDkz ) )/Dkz;
   _ddDkz:=( Sqr( Lk0.Vx-GLONASS[Work_Sat[1]]^[i-Ndop]^.Vx )+
    Sqr( Lk0.Vy-GLONASS[Work_Sat[1]]^[i-Ndop]^.Vy )+
    Sqr( Lk0.Vz-GLONASS[Work_Sat[1]]^[i-Ndop]^.Vz )+(dVxkz-_dVxk)*_dxk0+
               (dVykz-_dVyk)*_dyk0+(dVzkz-_dVzk)*_dzk0-Sqr( _dDkz ) )/_Dkz;
   for j:=1 to N_Work-1 do
     with GLONASS[Work_Sat[j+1]]^[i-Sat_Info[Work_Sat[j+1]].Nspace]^ do
      begin
         Ndop:=Sat_Info[Work_Sat[j+1]].Nspace;
         dxkj:=x-Lk_pro.x;
         dykj:=y-Lk_pro.y;
         dzkj:=z-Lk_pro.z;
         dVxkj:=Vx-Lk_pro.Vx;
         dVykj:=Vy-Lk_pro.Vy;
         dVzkj:=Vz-Lk_pro.Vz;
         _dxkj:=x-Lk0.x;
         _dykj:=y-Lk0.y;
         _dzkj:=z-Lk0.z;
         _dVxkj:=Vx-Lk0.Vx;
         _dVykj:=Vy-Lk0.Vy;
         _dVzkj:=Vz-Lk0.Vz;
         Vk:=dxkj*dVxkj+dykj*dVykj+dzkj*dVzkj;
         _Vk:=_dxkj*_dVxkj+_dykj*_dVykj+_dzkj*_dVzkj;
         Dkj:=Sqrt( Sqr( dxkj )+SQR( dykj )+SQR( dzkj ) );
         _Dkj:=Sqrt( Sqr( _dxkj )+SQR( _dykj )+SQR( _dzkj ) );
         dDkj:=Vk/Dkj;              _dDkj:=_Vk/_Dkj;
           SDE_Sat;
           dVxskj:=x*( sqsq+Omz2 )+2*Omz*Vy;
           dVyskj:=y*( sqsq+Omz2 )-2*Omz*Vx;
           dVzskj:=z*sqsq;
           ddDkj:=( Sqr( dVxkj )+Sqr( dVykj )+Sqr( dVzkj )+dxkj*( dVxskj-dVxk )+
                   dykj*( dVyskj-dVyk )+dzkj*( dVzskj-dVzk )-Sqr( dDkj ) )/Dkj;

         _ddDkj:=( Sqr( _dVxkj )+Sqr( _dVykj )+Sqr( _dVzkj )+_dxkj*( dVxskj-_dVxk )+
             _dykj*( dVyskj-_dVyk )+_dzkj*( dVzskj-_dVzk )-Sqr( _dDkj ) )/_Dkj;


         Rk[2*j-1]:=Dkj-Dkz+(Dkj*dDkj-Dkz*dDkz)/SLight
                     +(ddDkj*Sqr( Dkj )/Sqr(1.0-dDkj/SLight)-
                        ddDkz*Sqr( Dkz )/Sqr(1.0-dDkz/SLight))/4/SLight2;

         Rk[2*j]:=dDkj-dDkz+( Dkj*ddDkj+Sqr( dDkj )-ddDkz*Dkz-Sqr(dDkz))/SLight;

         Yk[2*j-1]:=_Dkj-_Dkz+(_Dkj*_dDkj-_Dkz*_dDkz)/SLight
                +(_ddDkj*Sqr( _Dkj )/Sqr(1.0-_dDkj/SLight)-
                _ddDkz*Sqr( _Dkz )/Sqr(1.0-_dDkz/SLight))/4/SLight2+Normal*Disp[2*j-1];

         Yk[2*j]:=_dDkj-_dDkz+( _Dkj*_ddDkj+Sqr( _dDkj )-
                        _Dkz*_ddDkz-Sqr( _dDkz ) )/SLight+Normal*Disp[2*j]
      end;
   for j:=1 to 2*(N_Work-1) do if Adaptation then
         begin
            dYk[j]:=Yk[j]-Rk[j]-Nu[j];
            Nu[j]:=i/( i+1 )*Nu[j]+( Yk[j]-Rk[j] )/( i+1 )
         end
      else dYk[j]:=Yk[j]-Rk[j];
   MMult_mnl( @Hk, @dYk, @dHk, 6, 20, 1, 6, 2*(N_Work-1), 1 );
   Lk.Vx:=dHk[1]+Lk_pro.Vx;  Lk.Vy:=Lk_pro.Vy+dHk[2];  Lk.Vz:=Lk_pro.Vz+dHk[3];
   Lk.x:=dHk[4]+Lk_pro.x;    Lk.y:=Lk_pro.y+dHk[5];    Lk.z:=Lk_pro.z+dHk[6]
end;

procedure Calk_TET( var TET: ar_6_6 );
var i, j: byte;
  t2, t3: float;
begin
   for i:=1 to 6 do for j:=1 to 6 do TET[i][j]:=0.0;
   t2:=Step*Step;         t3:=t2*Step;
   TET[1][1]:=tetX+4*Omz*Omz*t2*tetY/3;
   TET[1][2]:=Step*Omz*( tetY-tetX );
   TET[1][4]:=tetX*Step/2;
   TET[1][5]:=2*Omz*tetY*t2/3;
   TET[2][1]:=TET[1][2];
   TET[2][2]:=tetY+4*Omz*Omz*t2*tetX/3;
   TET[2][4]:=-2*Omz*tetX*t2/3;
   TET[2][5]:=tetY*Step/2;
   TET[3][3]:=tetZ;            TET[3][6]:=tetZ*Step/2;
   TET[4][1]:=tetX*Step/2;     TET[4][2]:=TET[2][4];
   TET[4][4]:=tetX*t2/3;       TET[5][1]:=TET[1][5];     TET[5][2]:=TET[2][5];
   TET[5][5]:=tetY*t2/3;       TET[6][3]:=TET[3][6];     TET[6][6]:=tetZ*t2/3;
   for i:=1 to 6 do for j:=1 to 6 do TET[i][j]:=TET[i][j]*Step
end;

Procedure Calk_Ak( var ak: ar_6_6 );
var
   dr3_dx, dr3_dy, dr3_dz, r3, r2: float;
                             l, k: byte;
                             sqsq: float;
begin
   for k:=1 to 6 do for l:=1 to 6 do ak[l][k]:=0;
   r2:=Sqr( Lk_old.x )+Sqr( Lk_old.y )+Sqr( Lk_old.z );
   r3:=r2*Sqrt( r2 );                     sqsq:=Omz*Omz-mz/r3;
   dr3_dx:=-3.0*Lk_old.x/r3/r2;
   dr3_dy:=-3.0*Lk_old.y/r3/r2;
   dr3_dz:=-3.0*Lk_old.z/r3/r2;
   ak[1][2]:=2.0*Omz;                     ak[1][4]:=sqsq-Lk_old.x*mz*dr3_dx;
   ak[1][5]:=-Lk_old.x*mz*dr3_dy;         ak[1][6]:=-mz*dr3_dz*Lk_old.x;
   ak[2][1]:=-2.0*Omz;                    ak[2][5]:=sqsq-Lk_old.y*mz*dr3_dy;
   ak[2][4]:=-Lk_old.y*mz*dr3_dx;         ak[2][6]:=-Lk_old.y*mz*dr3_dz;
   ak[3][4]:=-mz*Lk_old.z*dr3_dx;         ak[3][5]:=-mz*Lk_old.z*dr3_dy;
   ak[3][6]:=-mz/r3-mz*Lk_old.z*dr3_dz;   ak[4][1]:=1.0;
   ak[5][2]:=1.0;                         ak[6][3]:=1.0;
   for k:=1 to 6 do for l:=1 to 6 do Ak[k][l]:=Ak[k][l]*Step
end;

procedure Time_to_Str( var TimeSt: string; Tt: float );
var i: integer;
    a: float;
    st1: string[15];
begin
   TimeSt:='';
   i:=Round( Int( Tt/3600.0 ) );    str( i, st1 );
   while Length( st1 ) < 2 do st1:='0'+st1;
   TimeSt:=TimeSt+st1+':';
   Tt:=Tt-i*3600.0;
   i:=Round( Int( Tt/60.0 ) );      str( i, st1 );
   while Length( st1 ) < 2 do st1:='0'+st1;
   TimeSt:=TimeSt+st1+':';
   Tt:=Tt-60.0*i;
   str( Round( Tt ), st1 );
   while Length( st1 ) < 2 do st1:='0'+st1;
   TimeSt:=TimeSt+st1;
end;

procedure Calk_Noise( var Noise: Efm_P );
var
 mx, dx, sum, s: float;
     l, i, j, k: byte;
              e: array[1..6] of float;
              A: array[1..6] of array [1..6] of float;
begin
   for i:=1 to 6 do for j:=1 to 6 do A[i][j]:=0.0;
   for i:=1 to 6 do e[i]:=Normal;
   mx:=0;     dx:=0;
   for i:=1 to 6 do mx:=mx+e[i];                mx:=mx/6;
   for i:=1 to 6 do dx:=dx+Sqr( e[i]-mx );      dx:=dx/6;
   for i:=1 to 6 do e[i]:=( e[i]-mx )/Sqrt( dx );
   for i:=1 to 6 do
      begin
         for j:=1 to i do if j <> i then
                  begin
                     s:=0.0;
                     for k:=1 to j-1 do s:=s+A[j][k]*A[i][k];
                     A[i][j]:=( TET[j][i]-s )/A[j][j]
                  end
                         else {   if i = j then  }
                  begin
                     s:=0.0;
                     for k:=1 to i-1 do s:=s+Sqr( A[i][k] );
                     A[i][j]:=Sqrt( TET[i][j]-s )
                  end;
         sum:=0.0;
         for l:=1 to i do sum:=sum+A[i][l]*e[l];
         with Noise do Case i of
               1: Vx:=sum;     2: Vy:=sum;      3: Vz:=sum;
               4: x:=sum;      5: y:=sum;       6: z:=sum
            end
      end
end;

procedure ClrInfo;
var x, y : byte ;
begin
   x:=WhereX;          y:=WhereY;
   GotoXY( 1, 25 );    ClrEol;         GotoXY( x, y )
end;

procedure Mulk_Yes( sX, sY: byte;   {   Size  from  X, Y   }
                    cX, cY: byte;   {   Central Point      }
              BackC, TextC: byte );
var aX, aY, i: byte;
begin
   for i:=1 to Ti do
      begin
         aX:=Round( i*sX/Ti ) div 2;     aY:=Round( i*sY/Ti ) div 2;
         win( cX+2-aX, cY+1-aY, cX+2+aX, cY+1+aY, Black, Black );
         clrscr;
         win( cX-aX, cY-aY, cX+aX, cY+aY, BackC, TextC );
         clrscr;   delay( 30 )
      end
end;

procedure Mulk_No( sX, sY: byte;   {   Size  from  X, Y   }
                   cX, cY: byte;   {   Central Point      }
             BackC, TextC: byte );
var aX, aY, i: byte;
begin
   for i:=Ti downto 1 do
      begin
         aX:=Round( i*sX/Ti ) div 2;     aY:=Round( i*sY/Ti ) div 2;
         win( cX+2-aX, cY+1-aY, cX+2+aX, cY+1+aY, Black, Black );
         clrscr;
         win( cX-aX, cY-aY, cX+aX, cY+aY, BackC, TextC );
         clrscr;   delay( 15 );
         win( cX-aX, cY-aY, cX+2+aX, cY+2+aY, Bc, Tc );   clrscr;
      end;
end;

function ArcCos( arg: float ): float;
var x: float;
function Sign( x: float ): float;
begin
   if x < 0 then Sign:=-1.0 else Sign:=0.0
end;
begin
   if Arg = 0 then x:=pi/2 else
          x:=ArcTan( Sqrt( 1.0-Sqr( Arg ) )/Arg )+pi*Sign( Arg );
   x:=x*180/pi;
   if ( x < 0 ) and ( Abs( x ) > 180.0 ) then x:=360.0+x;
   ArcCos:=x
end;

function Trans_to_Rad( Sourse: Coord_Deg ): float;
begin
   with Sourse do Trans_to_Rad:=( deg+min/60.0+sec/3600.0 )*pi/180
end;

function Angle_Plase( var Plase, Sourse: Efm_P ): float;
var  a2, b2, c2, Gam: float;
begin
   with Sourse do
      begin
         a2:=Sqr( Plase.x )+Sqr( Plase.y )+Sqr( Plase.z );
         c2:=x*x+y*y+z*z;
         b2:=Sqr( x-Plase.x )+Sqr( y-Plase.y )+Sqr( z-Plase.z );
         Gam:=ArcCos( ( a2+b2-c2 )/2/Sqrt( a2*b2 ) );
      end;
    Angle_Plase:=Gam-90.0
end;

procedure Status_Line( s: string );
var x : byte ;
   begin
      Win( 1, 1, 80, 1, 7, 11 );    ClrScr;
      Write( s );                      x:=WhereX;
      Win( 1, 2, 80, 25, 1, 7 );       GotoXY( x, 2 );
      Kur( Off )
   end;

procedure Trans_To_Deg( Com: float; var Degree: Coord_Deg );
var f: float;
begin
   with Degree do
      begin
         Deg:=Round( Int( Com ) );
         f:=Frac( Com );              f:=f*60;
         Min:=Round( Int( f ) );
         f:=Frac( f );                f:=f*60;
         Sec:=f
      end
end;

procedure Trans_To_Geodezik( var Sourse: Efm_P; var Point: Point_Country );
var B_com, L_com, kdk: float;
begin
   with Sourse do
      begin
         B_com:=180/pi*ArcTan( z/Sqrt( ( x*x+y*y )*( 1.0-ez2 ) ) );
         Trans_To_Deg( B_com, Point.B );
         kdk:=180/pi*ArcTan( y/x );
         if x<0 then L_com:=180.0+kdk else
            if ( x>0 ) and ( y<=0 ) then L_com:=360.0+kdk else
               if ( x>0 ) and ( y>0 ) then L_com:=kdk else
                  if ( x=0 ) and ( y<0 ) then L_com:=270.0 else L_com:=90.0;
         Trans_To_Deg( L_com, Point.L );
         Point.H:=z/Sin( B_com*pi/180 )-az*( 1.0-ez2 )/
                 Sqrt( 1.0-ez2*Sin( B_com*pi/180 ) )
      end
end;


{******************           MAIN             ******************}

begin
   Status_Line( SL_Mes );
   Win( 1, 2, 80, 25, Bc, Tc );                   clrscr;
   Bc:=LightBlue;                                 Tc:=Yellow;
   Win( 1, 2, 80, 25, Bc, Tc );                   clrscr;
   Path:='';
   for i:=1 to 12 do
      begin
         Nsat[i]:=0;
         Work_Sat[i]:=0;
         with Sat_Info[i] do
            begin
               Npoint:=0;       Nspace:=0
            end
      end;
   Pa:=FSearch( 'nes_att.exe', GetEnv( 'PATH' ) );
   FSplit( FExpand( Pa ), Dir, Name, Ext );
   path:=Dir+'DATA\';
   write( '  Path to input files : ' );           Read_Str( path );
   InpF1:='nes.efm';                              InpF2:='glonass.efm';
   write( '    Input file with near-earth satellite ephemerid`s: ' );
   Read_Str( InpF1 );      assign( f1, Path+InpF1 );
   Success:=False;
   repeat
      OpenInpFile( Path, Success, Other, InpF1 )
   until Success;
   write( '    Input file with GLONASS  satellites ephemerid`s: ' );
   Read_Str( InpF2 );      assign( f2, Path+InpF2 );
   Success:=False;         Kur( Off );
   repeat
      OpenInpFile( Path, Success, Other, InpF2 )
   until Success;
   clrscr;
   Info( '   Reading  data  from  '+InpF1+'  and  '+InpF2+'  files . . .' );
   Kur( Off );
   readln( f1, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   DateSt:=Copy( Sstr, 1, 8 );
   with Data do
      begin
         Val( Copy( Sstr, 1, 2 ), day, cd );
         Val( Copy( Sstr, 4, 2 ), month, cd );
         Val( Copy( Sstr, 7, 2 ), year, cd )
      end;
   while Sstr <> '' do readln( f1, Sstr );
   while Sstr='' do readln( f1, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   Time1:=Copy( Sstr, 1, 8 );  Delete( Sstr, 1, 8 );    Del_Space( Sstr );
   while Sstr[1] = '-' do Delete( Sstr, 1, 1 );         Del_Space( Sstr );
   Time2:=Copy( Sstr, 1, 8 );  Delete( Sstr, 1, 8 );
   Delete( Sstr, 1, 1 );       Del_Space( Sstr );
   while Sstr[1] <> ' ' do Delete( Sstr, 1 , 1 );
   i:=1;                       Del_Space( Sstr );
   while Sstr[i] <> ' ' do Inc( i );      Dec( i );
   Val( Copy( Sstr, 1, i ), Step, cd );
   Sstr:='';      while Sstr='' do Readln( f1, Sstr );
   readln( f2, Sstr );
   Del_Space( Sstr );
   i:=1;
   while Sstr[i] <> ' ' do Inc( i );       Dec( i );
   Val( Copy( Sstr, 1, i ), N_Work, cd );
   Sstr:='';
   while Sstr='' do readln( f2, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   Time_int:=Copy( Sstr, 1, 8 );  Delete( Sstr, 1, 8 );    Del_Space( Sstr );
   while Sstr[1] = '-' do Delete( Sstr, 1, 1 );            Del_Space( Sstr );
   if Time1 <> Time_int then
      begin
         Info( '   Invalid time interval. Program halted. Press any key . . .' );
         AvrExit
      end;
   Time_Int:=Copy( Sstr, 1, 8 );     Delete( Sstr, 1, 8 );
   if Time2 <> Time_int then
      begin
         Info( '   Invalid time interval. Program halted. Press any key . . .' );
         AvrExit
      end;
   Delete( Sstr, 1, 1 );             Del_Space( Sstr );
   while Sstr[1] <> ' ' do Delete( Sstr, 1 , 1 );
   i:=1;                             Del_Space( Sstr );
   while Sstr[i] <> ' ' do Inc( i ); Dec( i );
   Val( Copy( Sstr, 1, i ), Step1, cd );
   if Step1 <> Step then
      begin
         Info( '   Invalid Step. Program halted. Press any key . . .' );
         AvrExit
      end;
   Val( Copy( Time1, 1, 2 ), hh, cd );
   Val( Copy( Time1, 4, 2 ), mm, cd );
   Val( Copy( Time1, 7, 2 ), ss, cd );
      tk1:=hh*3600.0+mm*60.0+ss*1.0;
   Val( Copy( Time2, 1, 2 ), hh, cd );
   Val( Copy( Time2, 4, 2 ), mm, cd );
   Val( Copy( Time2, 7, 2 ), ss, cd );
      tk2:=hh*3600.0+mm*60.0+ss*1.0;
   i:=0;
   Sstr:='';      while Sstr='' do Readln( f2, Sstr );

   N:=Round( ( tk2-tk1 )/Step ) + 1;
   if MemAvail < SizeOf( Efm_P )*N then begin
               Info( '  No Free Memory. Program halted. Press any key . . . ' );
               AvrExit
            end
                else for i:=1 to N do New( OKA[i] );
   for i:=1 to N do
      begin
         readln( f1, Sstr );
         with OKA[i]^ do
            begin
               Delete( Sstr, 1, 8 );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), Vx, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), Vy, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), Vz, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), x, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), y, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               Val( Sstr, z, cd )
            end;
      end;
   close( f1 );
   i:=0;  k:=1;
   Sstr:='';      while Sstr='' do Readln( f2, Sstr );
   while Sstr <> '' do
      begin
         Inc( i );
         Del_Space( Sstr );
         while Sstr[k] <> ' ' do Inc( k );   Dec( k );
         Val( Copy( Sstr, 1, k ), Nsat[i], cd );
         Delete( Sstr, 1 , k );
         k:=1;                                    Del_Space( Sstr );
         TimeSt:=Copy( Sstr, 1, 8);               Delete( Sstr, 1 , 8 );
         Del_Space( Sstr );

         Val( Copy( TimeSt, 1, 2 ), hh, cd );
         Val( Copy( TimeSt, 4, 2 ), mm, cd );
         Val( Copy( TimeSt, 7, 2 ), ss, cd );
            ttt1:=hh*3600.0+mm*60.0+ss*1.0;
         Sat_Info[i].Nspace:=Round( ( ttt1-tk1 )/Step );
         Val( Copy( Sstr, 1, 2 ), hh, cd );
         Val( Copy( Sstr, 4, 2 ), mm, cd );
         Val( Copy( Sstr, 7, 2 ), ss, cd );
            ttt2:=hh*3600.0+mm*60.0+ss*1.0;
         Sat_Info[i].Npoint:=Round( ( ttt2-ttt1 )/Step )+1;

         if MaxAvail < SizeOf( Sat_ar ) then begin
               Info( '  No Free Memory. Program halted. Press any key . . . ' );
               AvrExit
            end
              else New( GLONASS[i] );
         if MaxAvail < SizeOf( Efm_P )*Sat_Info[i].Npoint then begin
              Info( '  No Free Memory. Program halted. Press any key . . . ' );
              AvrExit
            end
             else for j:=1 to Sat_Info[i].Npoint do New( GLONASS[i]^[j] );
         Readln( f2, Sstr );
      end;
   Sstr:='';
   while Sstr='' do readln( f2, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   with Data1 do
      begin
         Val( Copy( Sstr, 1, 2 ), day, cd );
         Val( Copy( Sstr, 4, 2 ), month, cd );
         Val( Copy( Sstr, 7, 2 ), year, cd )
      end;
   if ( Data.day <> Data1.day ) or ( Data.month <> Data1.month ) or
       ( Data.year <> Data1.year ) then
          begin
             Info( '   Invalid date. Program halted. Press any key . . .' );
             AvrExit
          end;

   for j:=1 to N_Work do
      begin
         Sstr:='';
         while Sstr='' do readln( f2, Sstr );
         for i:=1 to Sat_Info[j].Npoint do with GLONASS[j]^[i]^ do
            begin
               Readln( f2, Sstr );
               Delete( Sstr, 1, 8 );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), Vx, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), Vy, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), Vz, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), x, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), y, cd );
               Delete( Sstr, 1, k );     Del_Space( Sstr );   k:=1;
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), z, cd )
            end;
         Sstr:='';
         if j = N_Work then else
            begin
               while Sstr = '' do readln( f2, Sstr );
            end
      end;
   close( f2 );
   Clrscr;
   write( '    Use Adaptation Kalman-filter? (Y/N) ' );
   Kur( Off );            ch:=ReadKey;                Clrscr;
   if UpCase( ch ) = 'Y' then Adaptation:=True else Adaptation:=False;
   MNul( @Disp, 20 );       MNul( @Nu, 20 );       MNul( @dYk, 20 );
   MNul( @Yk, 20 );         MNul( @Rk, 20 );       MNul( @Nks, 20*20 );
   MNul( @AA, 20*20 );      MNul( @Ck, 6*20 );     MNul( @Cktr, 6*20 );
   MNul( @Hk, 6*20 );       MNul( @AAtr, 6*20 );   MNul( @ei, 6*6 );
   MNul( @Fk, 6*6 );        MNul( @Fktr, 6*6 );    MNul( @TET, 6*6 );
   MNul( @Ak, 6*6 );        MNul( @Eks, 6*6 );     MNul( @Eks_pr, 6*6 );
   MNul( @Aa6, 6*6 );

   MNul( @Lk, 6 );          Lk0:=Lk;
   if N_Work > 10 then ik:=10 else ik:=N_Work;
   for i:=1 to ik do
      begin
         Nks[2*i-1][2*i-1]:=1.0;
         Nks[2*i][2*i]:=0.02
      end;
   writeln( ' Input RMS error for each satellites . . . ' );
   for i:=1 to 2*ik do
      begin
         write( '   Sat N ', ((i-1) div 2 + 1 ):2, ' ........ ' );
         x:=WhereX;        y:=WhereY;
         if ( i div 2 ) = ( i / 2 ) then Mes:='( Speed, [m/s] )'
                                    else Mes:='( Length, [m] )';
         GotoXY( x+15, y );         write( Mes );
         GotoXY( x, y );
         Read_Num( Nks[i][i] );     Disp[i]:=Nks[i][i]
      end;
   for i:=1 to 2*ik do Nks[i][i]:=Sqr( Nks[i][i] );
   clrscr;
   tetX:=1.0;      tetY:=2.0;     tetZ:=1.0;
   for i:=1 to 3 do
      begin
         write( ' Input Tet[', i, '][', i, '] . . . ' );
         x:=WhereX;        y:=WhereY;
         Mes:='( * E-6, [m2/s3] )';
         GotoXY( x+15, y );    write( Mes );
         GotoXY( x, y );
         Case i of
               1: Read_Num( tetX );
               2: Read_Num( tetY );
               3: Read_Num( tetZ )
            end
      end;
   tetX:=tetX*1.0E-6;     tetY:=tetY*1.0E-6;      tetZ:=tetZ*1.0E-6;
   clrscr;


   ik:=N_Work;      N_Work:=0;    betmax:=0.0;
   m:=1;
   for l:=1 to ik do with Sat_Info[l] do
      if Nspace = 0 then
         begin
            Work_Sat[m]:=l;   Inc( m );    Inc( N_Work )
         end;
   for l:=1 to N_Work do
      begin
        bet:=Angle_Plase( OKA[1]^, GLONASS[Work_Sat[l]]^[1]^ );
        if bet > betmax then
           begin
              betmax:=bet;                    cd:=Work_Sat[1];
              Work_Sat[1]:=Work_Sat[l];       Work_Sat[l]:=cd;
           end
      end;
(*   for i:=1 to N_Work do
      begin
         Nks[2*i-1][2*i-1]:=Nks_ar[i].a1;
         Nks[2*i][2*i]:=Nks_ar[i].a2
      end;         *)


   InpF1:='estimate.nes';                                 InpF2:='error.nes';
   write( '  Output file name ( for estimate ) : ' );     Read_Str( InpF1 );
   write( '  Output file name (  for  error  ) : ' );     Read_Str( InpF2 );
   clrscr;

   MInverse_nn( @Nks, 20, 2*( N_Work-1 ) );
   Lk0:=OKA[1]^;          Lk_old:=OKA[1]^;     Lk_pro:=Lk_old;
   Calk_TET( TET );
      Calk_Ck( Ck );
      Mtransp_mn( @Ck, @Cktr, 20, 6, 2*( N_Work-1 ), 6 );
      MMult_mnl( @Cktr, @Nks, @AAtr, 6, 20, 20, 6, 2*(N_Work-1), 2*(N_Work-1) );
      MMult_mnl( @AAtr, @Ck, @Eks, 6, 20, 6, 6, 2*( N_Work-1 ), 6 );
      MInverse_nn( @Eks, 6, 6 );
   for i:=1 to 6 do ei[i][i]:=1.0;
   RK_int( Lk_old, Lk_pro, Step );         Lk:=Lk_old;
   MNul( @Delt_Lk, 6 );                    MNul( @Disp_Lk, 6 );

   Val( Copy( Time1, 1, 2 ), hh, cd );     Val( Copy( Time1, 4, 2 ), mm, cd );
   Val( Copy( Time1, 7, 2 ), ss, cd );
   Time_beg:=hh*3600.0+mm*60.0+ss*1.0;
   Mulk_Yes( 30, 6, 40, 12, Red, Black );
   Win( 25, 9, 55, 15, Red, Black );
   GotoXY( 10, 2 );    Write( '  Execution' );
   GotoXY( 8, 5 );    Write( ' Processed  00  %' );
   Kur( Off );
   assign( f1, Path+InpF1 );      assign( f2, Path+InpF2 );
   Rewrite( f1 );                 Rewrite( f2 );
   writeln( f1, '     Velosity and coordinate estimate`s for date: ',
               DateSt, '.' );
   writeln( f1 );
   writeln( f2, '     Velosity and coordinate RMS-error estimate`s for date: ',
               DateSt, '.' );
   writeln( f2 );
   writeln( f1, '     Time interval: ', Time1, ' -  ', Time2, '. Step ',
               Round( Step ), ' sec.' );            Writeln( f1 );
   writeln( f2, '     Time interval: ', Time1, ' -  ', Time2, '. Step ',
               Round( Step ), ' sec.' );            writeln( f2 );
   writeln( f1, N_Work:7, ' work satellites.' );   writeln( f1 );
   writeln( f2, N_Work:7, ' work satellites.' );   writeln( f2 );
   writeln( f1, '  Time    Vx, [m/s]  Vy, [m/s]  Vz, [m/s]   '+
                                '   x, [m]       y, [m]       z, [m]' );
   writeln( f2, '  Time    Vx, [m/s]  Vy, [m/s]  Vz, [m/s]   '+
                                '   x, [m]       y, [m]       z, [m]' );
   Time:=Time_beg+Step;
   Time_to_Str( TimeSt, Time );
   with Disp_Lk do
      begin
         Vx:=Sqrt( Eks[1][1] );    Vy:=Sqrt( Eks[2][2] );
         Vz:=Sqrt( Eks[3][3] );    x:=Sqrt( Eks[4][4] );
         y:=Sqrt( Eks[5][5] );     z:=Sqrt( Eks[6][6] );
      end;
   with Delt_Lk do writeln( f1, TimeSt, Vx:11:7, Vy:11:7, Vz:11:7,
                                            x:13:7,  y:13:7,  z:13:7 );
   with Disp_Lk do writeln( f2, TimeSt, Vx:11:7, Vy:11:7, Vz:11:7,
                                            x:13:7,  y:13:7,  z:13:7 );
   for i:=2 to N do
      begin
         N_Work:=0;     betmax:=0.0;
         m:=1;
         for l:=1 to ik do with Sat_Info[l] do
            if (Nspace < i) and ((Nspace+Npoint) >= i) then
               begin
                  Work_Sat[m]:=l;   Inc( m );     Inc( N_Work )
               end;
         for l:=1 to N_Work do
            begin
               bet:=Angle_Plase( OKA[i]^,
                     GLONASS[Work_Sat[l]]^[i-Sat_Info[Work_Sat[l]].Nspace]^ );
               if bet > betmax then
                  begin
                     betmax:=bet;                 cd:=Work_Sat[1];
                     Work_Sat[1]:=Work_Sat[l];    Work_Sat[l]:=cd
                  end
            end;
         Calk_Ck( Ck );                  MNul( @Cktr, 6*20 );
         Mtransp_mn( @Ck, @Cktr, 20, 6, 2*( N_Work-1 ), 6 );
         Calk_Ak( Ak );                  MNul( @Fk, 6*6 );
         MAdd_mn( @ei, @Ak, @Fk, 6, 6, 6, 6 );
                                         MNul( @Fktr, 6*6 );
         Mtransp_mn( @Fk, @Fktr, 6, 6, 6, 6 );
                                         MNul( @Aa6, 6*6 );
         MMult_mnl( @Fk, @Eks, @Aa6, 6, 6, 6, 6, 6, 6 );
         MNul( @Fk, 6*6 );               MNul( @Eks_pr, 6*6 );
         MMult_mnl( @Aa6, @Fktr, @Fk, 6, 6, 6, 6, 6, 6 );
         MAdd_mn( @Fk, @TET, @Eks_pr, 6, 6, 6, 6 );
         MInverse_nn( @Eks_pr, 6, 6 );   MNul( @AAtr, 6*20 );
         MMult_mnl( @Cktr, @Nks, @AAtr, 6, 20, 20, 6, 2*(N_Work-1), 2*(N_Work-1) );
                                         MNul( @Eks, 6*6 );
         MMult_mnl( @AAtr, @Ck, @Eks, 6, 20, 6, 6, 2*( N_Work-1 ), 6 );
         MAdd_mn( @Eks_pr, @Eks, @Eks, 6, 6, 6, 6 );
         MInverse_nn( @Eks, 6, 6 );
         with Disp_Lk do
            begin
               Vx:=Sqrt( Eks[1][1] );    Vy:=Sqrt( Eks[2][2] );
               Vz:=Sqrt( Eks[3][3] );    x:=Sqrt( Eks[4][4] );
               y:=Sqrt( Eks[5][5] );     z:=Sqrt( Eks[6][6] );
            end;
         MNul( @AAtr, 6*20 );            MNul( @Hk, 6*20 );
         MMult_mnl( @Eks, @Cktr, @AAtr, 6, 6, 20, 6, 6, 2*(N_work-1) );
         MMult_mnl( @AAtr, @Nks, @Hk, 6, 20, 20, 6, 2*(N_Work-1), 2*(N_Work-1) );
         Calk_Noise( Noise );
         with Lk0 do
            begin
               Vx:=OKA[i]^.Vx+Noise.Vx;       Vy:=OKA[i]^.Vy+Noise.Vy;
               Vz:=OKA[i]^.Vz+Noise.Vz;       x:=OKA[i]^.x+Noise.x;
               y:=OKA[i]^.y+Noise.y;          z:=OKA[i]^.z+Noise.z
            end;
         Filtering( Lk, Lk0 );                Lk_old:=Lk;
         with Delt_Lk do
            begin
               Vx:=OKA[i]^.Vx-Lk.Vx;          Vy:=OKA[i]^.Vy-Lk.Vy;
               Vz:=OKA[i]^.Vz-Lk.Vz;          x:=OKA[i]^.x-Lk.x;
               y:=OKA[i]^.y-Lk.y;             z:=OKA[i]^.z-Lk.z
            end;
         Rk_int( Lk_old, Lk_pro, Step );
         Time:=Time_beg+Step*( i-1 );
         Time_to_Str( TimeSt, Time );
         with Delt_Lk do writeln( f1, TimeSt, Vx:11:7, Vy:11:7, Vz:11:7,
                                                  x:13:7,  y:13:7,  z:13:7 );
         with Disp_Lk do writeln( f2, TimeSt, Vx:11:7, Vy:11:7, Vz:11:7,
                                                  x:13:7,  y:13:7,  z:13:7 );
         GotoXY( 19, 5 );                     write( Round( i*10/N*10 ):3 );
         Kur( Off )
      end;
   Mulk_No( 30, 6, 40, 12, Red, Black );
   Close( f1 );       Close( f2 );
   for i:=1 to N do Dispose( OKA[i] );
   for j:=1 to ik do
      begin
         for i:=1 to Sat_Info[j].Npoint do Dispose( GLONASS[j]^[i] );
         Dispose( GLONASS[j] )
      end;
   Win( 1, 1, 80, 25, Black, White );  AvrSound;    clrscr
end.
