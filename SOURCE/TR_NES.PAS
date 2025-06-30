{$L egavga.obj}
{$L cga.obj}
{$V-}
program TR_NES;    {      Trace Near-Earth Satellite     }
uses Crt, Dos, Graph;

label Start;
const   RK_Step = 1.0; { Step for RKGS - subroutine, [c] }
             Ti = 9;
        Century = 1900;
         SL_Mes = '   Trass Near-Earth Satellite by Kepler parameters. ' +
                     'V-2.0.       Kharkov, 1995.';

type Point_Str = ^String;
         Float = Real;
         OnOff = ( On, Off );
     StateType = ( Passive, Active, Select ) ;
        Signum = ( Kepler, Ephemerids, Coord, Other );
        ForMAP = ( NES, NSS );
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

const
    Monthes: array [1..12] of byte=( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );
     Omz = 0.7292115147*1E-4;      { [рад/c] }
      Sb = 1E-3;
      H0 = 9.0e+5;                 { [m] }
     C20 = -1082.637e-6;
     C30 = 2.54e-6;
     C40 = 1.619e-6;
     C22 = 1.54e-6;
     D22 = -0.88e-6;
      mz = 0.3986012*1E+15;   { Гравитационный потенциал Земли, [м3/с2] }
     ez2 = 6.6934216*1E-3;    { Квадрат Земного эксцентриситета }
      az = 6378136.0;         { Экваториальный радиус Земли, [м] }
var
  IOold, Gm, Count, j, x, y, Size, x0, y0, i, Gd, cd: integer;
          NC, NChoise, Tc, Bc, Nsel, Nvs, hh, mm, ss: byte;
                      MaxX, MaxY, Year_kep, Year_now: word;
      cx, cy, Tcr, Time, Step, tk1, tk2, Bet, L0, B0: float;
                        DateSt, Time1, Time2, TimeSt: string[10];
                                    Path, InpF, Name: string[79];
                   Success, N_Vis, Visible, Selected: boolean;
                          Point_efm, Point_Gr, Point: Efm_P;
                                    Plase_Sat, Plase: Point_Country;
                                         Today, Data: Date;
                                            Sum_Days: longint;
                                              Sourse: Kepler_P;
                                               ch, c: char;
                                               f, f1: text;
                                                Sign: Signum;
                                                Smap: ForMAP;
                                              Ar_Str: array[0..50] of Point_Str;


function SD( var Data, Today: Date ): longint;
var Year_kep, Year_now, Nvs, Sum_Days: longint;
begin
   Year_kep:=Century+Data.year;           Nvs:=0;
   Year_now:=Century+Today.year;          Sum_Days:=0;
   while Year_kep < Year_now do
      begin
         if ( Year_kep div 4 ) = ( Year_kep/4 ) then Inc( Nvs );
         Inc( Year_kep )
      end;
   Year_kep:=Century+Data.year;
   if ( ( Year_now div 4 ) = ( Year_now/4 ) ) and ( Today.month > 2 ) then Inc( Nvs );
   for i:=1 to Data.month-1 do Sum_Days:=Sum_Days+Monthes[i];
   Sum_Days:=Sum_Days+Data.Day-1;
   if ( ( Year_kep div 4 ) = ( Year_kep/4 ) ) and ( Data.month > 2 ) then
                         Sum_Days:=Sum_Days+1;
   if Year_kep = Year_now then Sum_Days:=-Sum_Days-1
                          else Sum_Days:=365-Sum_Days-1;
   Inc( Year_kep );
   while Year_kep < Year_now do
      begin
         Sum_Days:=Sum_Days+365;     Inc( Year_kep )
      end;
   Sum_Days:=Sum_Days+Nvs;
   for i:=1 to Today.month-1 do Sum_Days:=Sum_Days+Monthes[i];
   Sum_Days:=Sum_Days+Today.day-1;
   SD:=Sum_Days
end;

procedure Kur( Signum: OnOff );
var Regs: Registers;
begin
   with Regs do
      begin
         AH:=2;     DL:=0;   BH:=0;
         Case Signum of
               Off:  DH:=25;
                On:  DH:=1
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
            clreol;        readln( s1 );       Exit:=True
         end
   until Exit;
   if s1='' then else s:=s1;      GotoXY( x, y );    writeln( s );
   TextColor( Tc )
end;

procedure Read3num ( var v1, v2, v3: byte; var TimeSt: string );
var  c: char;
begin
   Read_Str( TimeSt );                   Del_Space( TimeSt );
   if Timest[1]='d' then c:='.' else c:=':';
   case Length( TimeSt ) of
         2:     TimeSt:=TimeSt+':00:00';
         4:     TimeSt:=TimeSt+'0:00';
         5:     TimeSt:=TimeSt+':00'
      end;
         Val( copy( TimeSt, 1, 2 ), v1, cd );     TimeSt[3]:=c;
         Val( copy( TimeSt, 4, 2 ), v2, cd );     TimeSt[6]:=c;
         Val( copy( TimeSt, 7, 2 ), v3, cd );
   TextColor( Tc )
end;

procedure Read_Num( var Num: float );
const ww = 6;
var
    x, y, cd, Wide: integer;
            s1, s2: string[45];
              Exit: boolean;
begin
   x:=WhereX;         y:=WhereY;
   Str( Num:ww:( ww-4 ), s2 );
   TextColor( LightRed + Blink );
   write( s2 );       GotoXY( x, y );
   Exit:=False;       TextColor( White );
   repeat
      if keypressed then
         begin
            clreol;        readln( s1 );       Exit:=True
         end
   until Exit;
   Wide:=Length( s1 );
   if Wide < ww then Wide:=ww;
   GotoXY( x, y );
   if s1='' then writeln( s2 ) else
      begin
         clreol;    Val( s1, Num, cd );   writeln( s1:Wide )
      end;
   TextColor( Tc )
end;

procedure ClrInfo;
var x, y : byte ;
begin
   x:=WhereX;          y:=WhereY;
   GotoXY( 1, 25 );    ClrEol;         GotoXY( x, y )
end;

procedure InfoMap ( s : string );
var Color: Word;
begin
   SetFillStyle( 1, White );                Color:=GetColor;
   Bar( 235, maxY-35, 418, maxY-26 );       SetColor( Black );
   SetTextJustify( CenterText, TopText );
   OutTextXY( 328, maxY-34, s );            GotoXY( 1, 1 );
   SetColor( Color );
   SetTextJustify( LeftText, TopText );     SetFillStyle( 1, Black )
end;

procedure Map( var Single: ForMAP );
      const n: array [1..9] of byte= ( 0, 3, 37, 53, 63, 79, 91, 95, 113 ) ;
            L: array [1..113] of integer=( -180, -170, -180, 180, 114,
               110, 45, 20, 5, 10, -4, -2, -8, -10, -7, 5, 10, 22, 28, 32, 37,
               42, 28, 27, 37, 32, 43, 68, 77, 90, 105, 122, 142, 140, 155,
               157, 180, 10, -7, -17, -18, -8, 8, 14, 12, 20, 30, 41, 38, 50,
               43, 32, 20, 143, 139, 133, 125, 107, 116, 133, 143, 150,
               154, -60, -130, -160, -168, -160, -140, -120, -105, -80,
               -88, -97, -97, -80, -80, -32, -80, -72, -80, -77, -82, -71,
               -77, -65, -68, -62, -40, -34, -58, -20, -55, -42, -20,
               -180, -160, -140, -128, -100, -100, -75, -58, -58, -40, 0,
               53, 75, 82, 140, 170, 165, 180 ) ;
            B: array [1..113] of integer=( 63, 66, 68, 68, 75, 78, 67,
               72, 60, 55, 48, 43, 43, 37, 36, 40, 43, 40, 41, 47, 45, 42, 41,
               36, 36, 32, 14, 25, 8, 22, 4, 24, 48, 57, 60, 50, 63, 38, 36,
               22, 14, 5, 3, -5, -17, -35, -32, -15, -5, 13, 10, 32, 32, -11,
               -17, -12, -15, -23, -35, -33, -39, -37, -25, 83, 70, 71, 66,
               56, 59, 35, 19, 10, 22, 19, 28, 25, 32, 48, 75, 12, 10, 5, -5,
               -19, -50, -57, -50, -40, -22, -7, 8, 84, 77, 63, 70, -79,
               -77, -75, -73, -74, -72, -67, -62, -75, -77, -64, -67, -70,
               -67, -67, -72, -78, -79 ) ;
var i, j, k, x1, x2, y1, y2 : integer ;
                        Col : byte;
                          s : string  ;
begin
    if Gd=CGA then SetColor( White ) else SetColor( Yellow );
    Rectangle( 0, 0, maxX, maxY );
    Rectangle( 28, 0, maxX-28, maxY-47 );
    x0:=maxX div 2;                 y0:=( MaxY-47 ) div 2;
    cx:=( maxX-57 )/360.0;          cy:=-( MaxY-46 )/180.0;
    k:=-180;
    if Gd=CGA then SetColor( White ) else SetColor( Green );
    if Gd=CGA then Col:=White else Col:=Green;
    for i:=0 to 18 do
       begin
          x1:=Round( x0+cx*k );     y1:=1;
          if i <> 18 then
             while y1 <= ( maxY-48 ) do
                begin
                   PutPixel( x1, y1, Col );   Inc( y1, 4 )
                end;
          x1:=x1-16;
          Str( k:4, s );   OutTextXY( x1, maxY-45, s );   k:=k+20
       end;
    k:=-60;
    for i:=1 to 5 do
       begin
          y1:=Round( y0+cy*k )-1;             x1:=28;
          while x1 <= 609 do
             begin
                PutPixel( x1, y1, Col );      Inc( x1, 8 )
             end;
          y1:=y1-4;
          Str( k:3, s );  OutTextXY( 2, y1, s );   k:=k+30
       end;
    Str( k:3, s );        OutTextXY( 2, 2, s );    k:=-90;
    Str( k:3, s );        OutTextXY( 2, maxY-53, s );
    if Gd=CGA then SetColor( White ) else SetColor( LightCyan );
    for i:=1 to 8 do
       begin
          for j:=n[i]+1 to n[i+1]-1 do
             begin
                x1:=Round( x0+cx*L[j] );
                y1:=Round( y0+cy*B[j] );
                x2:=Round( x0+cx*L[j+1] );
                y2:=Round( y0+cy*B[j+1] );
                Line( x1, y1, x2, y2 )
             end;
          if i <> 8 then
             begin
                x1:=Round( x0+cx*L[n[i]+1] );
                y1:=Round( y0+cy*B[n[i]+1] );
                Line( x1, y1, x2, y2 )
             end
       end;
    if Gd=CGA then SetColor( White ) else SetColor( LightRed );
    Rectangle( 28, maxY-37, 611, MaxY );
    Rectangle( 150, maxY-35, 217, maxY-26 );
    Rectangle( 150, maxY-24, 217, maxY-15 );
    OutTextXY( 35, maxY-23, 'Curent Time -' );
    OutTextXY( 35, maxY-34, '   Date - ' );
    InfoMap( 'E-exit / S-stop' );
    Case Single of
          NES: OutTextXY( 34, maxY-11, 'Satellite number : ' );
          NSS: begin
                  OutTextXY( 34, maxY-11, 'Number of Visible Sat : ' );
                  SetColor( Red );     OuttextXY( 252, maxY-17, '*' );
                  SetColor( Yellow );  OuttextXY( 258, maxY-17, ' - NES' )
               end
       end;
    if Gd = CGA then SetColor( White ) else SetColor( Yellow );
    SetFillStyle( 1, 1 );
    PieSlice( 326, maxY-8, 0, 360, 5 );
    OutTextXY( 335, maxY-24, '-invisible' );
    OutTextXY( 335, maxY-11, '-  visible' );
    Circle( 326, maxY-21, 5 );
    SetTextJustify( LeftText, TopText );
end;

function Trans_to_Rad( Sourse: Coord_Deg ): float;
begin
   with Sourse do Trans_to_Rad:=( deg+min/60.0+sec/3600.0 )*pi/180
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
   ArcCos:=x;
end;

procedure Win ( x1, y1, x2, y2, Bc, Tc : byte ) ;
begin
   Window( x1, y1, x2, y2 );
   TextBackground( Bc );          TextColor( Tc )
end;

procedure Info( s: string ) ;
var x, y : byte ;
   begin
      Win( 1, 25, 80, 25, LightGray, Red );    ClrScr;
      Write( ' '+s );                          x:=WhereX;     y:=WhereY;
      Win( 1, 2, 80, 25, Bc, Tc );     GotoXY( x, y );
      Kur( Off )
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

procedure AvrSound;
const Note = 100;
var i: integer;
begin
   for i:=1 to Note do
      begin
         Sound( i*2 );  delay( 2 )
      end;
   delay( 150 );
   for i:=Note downto 1 do
      begin
         Sound( i*2 );  delay( 2 )
      end;
   Nosound
end;

procedure SuccessSound;
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

procedure OpenInpFile ( path: string; var Success: boolean; ch: char );
var c: char;
   begin
      ClrScr;
      Assign( f, path );
{$I-}
      case UpCase( ch ) of
            'I': Reset( f );
            'O': Rewrite( f )
         end;
{$I+}
      if IOResult <> 0 then
         begin
            Success:=False;
            Info( 'Data file ( '+path+' ) not found . . . Press any key' );
            AvrSound;                c:=Readkey
         end
               else begin Success:=True;
                 Info( 'Loading data file ( '+path+' ) . . .' )
                    end
   end;

procedure ReadKeplerPrm;
const Na='Kepler.prm';
var  j : byte;
    Dir: DirStr;       Name: NameStr;       Ext: ExtStr;
     Pa: PathStr;        st: string;
begin
   Pa:=FSearch( 'tr_nes.exe', GetEnv( 'PATH' ) );
   FSplit( FExpand( Pa ), Dir, Name, Ext );
   path:=Dir+'DATA\';
   repeat
      ClrScr;               Write( '  Path to data ( '+Na+' ) file : ' );
      Read_Str( path );     OpenInpFile( path+Na, Success, 'i' )
   until Success;
   Readln( f );           j:=0;
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
   if NChoise=0 then Info( '   Error : '+Na+' is empty . . . Press any key' )
end;

procedure Trans_To_Deg( Com: float; var Degree: Coord_Deg );
var f: float;
begin
   with Degree do
      begin
         Deg:=Round( Int( Com ) );
         f:=Frac( Com )*60;
         Min:=Round( Int( f ) );
         f:=Frac( f )*60;
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

procedure Trase_Kepler( Sourse: Kepler_P; Time: float; var Point: Efm_P );
var
    dMk, Mk, Ek, CosEk, SinEk, Znam, rk, CosVk, SinVk, SinW, CosW:  float;
        SinUk, CosUk, CosOm, SinOm, Cosi, Sini, CosMk, Cos2Mk, e2:  float;
                               dEk, drk, dVk, dCosUk, dSinUk, drr:  float;
function Kepler_Eq_MPP( e, Ek0: float ): float;
var Ekk, E0: float;
begin
   Ekk:=Ek0;
   repeat
      E0:=Ekk;
      Ekk:=Mk+e*Sin( E0 );
   until ( Abs( Ekk-E0 ) ) < 1e-10;
   Kepler_Eq_MPP:=Ekk
end;

begin        {  Tpase_Kepler  }
   with Sourse do
      begin
         dMk:=Sqrt( mz/a/a/a );
         Mk:=dMk*( Time-tp );
         Mk:=2*pi*Frac( Mk/pi/2 );
         Ek:=Kepler_Eq_MPP( e, Mk );
         CosEk:=Cos( Ek );           SinEk:=Sin( Ek );
         Znam:=1.0-e*CosEk;
         rk:=a*Znam;
         CosVk:=( CosEk-e )/Znam;    SinVk:=Sqrt( 1-Sqr( e ) )*SinEk/Znam;
         SinW:=Sin( w );             CosW:=Cos( w );
         SinUk:=SinW*CosVk+CosW*SinVk;
         CosUk:=CosW*CosVk-SinW*SinVk;
         CosOm:=Cos( Om );           SinOm:=Sin( Om );
         Cosi:=Cos( i );             Sini:=Sin( i )
      end;
   with Point do
      begin
         x:=rk*( CosUk*CosOm-SinUk*SinOm*Cosi );
         y:=rk*( CosUk*SinOm+SinUk*CosOm*Cosi );
         z:=rk*SinUk*Sini
      end;
   with Sourse do
      begin
         CosMk:=Cos( Mk );       Cos2Mk:=Cos( 2.0*Mk );
         e2:=e*e;
         dEk:=dMk*( 1.0+e*CosMk+e2*Cos2Mk+e*e2/8*( 9*Cos( 3*Mk )-CosMk )+
                      e2*e2/6*( 8*Cos( 4*Mk )-2*Cos2Mk ) );
         drk:=a*e*Sin( Ek )*dEk;
         dVk:=dEk*Sqrt( 1.0-e2 )/Znam;
         dCosUk:=-SinUk*dVk;     dSinUk:=CosUk*dVk
      end;
   with Point do
      begin
         drr:=drk/rk;
         Vx:=drr*x+rk*( dCosUk*CosOm-dSinUk*SinOm*Cosi );
         Vy:=drr*y+rk*( dCosUk*SinOm+dSinUk*CosOm*Cosi );
         Vz:=drr*z+rk*dSinUk*Sini
      end
end;      {   Trase_Kepler    }

procedure Choise( NChoise: byte; var Str_Choise: string; var NC: byte );
var i, kmax, xx, yy, j, k, xcur, ycur: byte;
                                 Exit: boolean;
                                    c: char;
procedure Menu ( nMelm : byte ; State : StateType ) ;
var Bcm, Tcm : byte ;
begin
   case State of
         Passive : begin Bcm:=Blue;     Tcm:=LightGreen    end;
         Select  : begin Bcm:=Green;    Tcm:=LightRed      end;
         Active  : begin Bcm:=White;    Tcm:=Red           end
      end;
   Win( xx, yy+nMelm-1, xx+kmax+1, yy+nMelm-1, Bcm, Tcm );    ClrScr;
   Write( ' '+( Ar_Str[nMelm] )^ );            Win( 1, 2, 80, 25, Bc, Tc );
   GotoXY( Xcur, Ycur );
   Kur( Off )
end;

begin    {   Choise   }
   kmax:=1;      Xcur:=WhereX;         Ycur:=WhereY;       i:=1;
   if NC = 0 then
      begin
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
               Exit:=False;                  i:=1;
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
                              Menu( j, Passive );
                              case Ord( c ) of
                                    80: if j=Nchoise then j:=1 else Inc( j );
                                    72: if j=1 then j:=Nchoise else Dec( j );
                                    81: j:=Nchoise;
                                    73: j:=1;
                                 end;
                              Menu( j, Active )
                           end
                              else  if c=#13 then
                                 begin
                                    Exit:=True; Menu( j , Select ); Delay( 200 )
                                 end
                     end
               until Exit
            end;      {   if i > 1   }
         Str_Choise:=( Ar_Str[j] )^;
         NC:=j
      end
           else Str_Choise:=( Ar_Str[NC] )^;   { if  NC <> 0 }
   for i:=0 to Nchoise do Dispose( Ar_Str[i] );
end;      {   of  Choise    }

procedure Read_Data_Kepler( var Sourse:   Kepler_P;
                            var Date_Kep: Date;
                            var NC:       byte );
var
    Str_Choise: string;
  deg, min, cd: integer;
       Ab, sec: float;

begin
   ReadKeplerPrm;
   clrscr;
   Info( '   Choose working user ( keys: Up, Dn-move; ENTER-choose )' );
   Choise( NChoise, Str_Choise, NC );
   delete( Str_Choise, 1, 4 );
   while Str_Choise=' ' do Delete( Str_Choise, 1, 1 );
   with Date_Kep do
      begin
         Val( Copy( Str_Choise, 1, 2 ), day, cd );
         Val( Copy( Str_Choise, 3, 2 ), month, cd );
         Val( Copy( Str_Choise, 5, 2 ), year, cd )
      end;
   Delete( Str_Choise, 1, 6 );    Del_Space( Str_Choise );
   with Sourse do
      begin
         Val( Copy( Str_Choise, 1, 10 ), a , cd );
         Delete( Str_Choise, 1, 10 );              Del_Space( Str_Choise );
         Val( Copy( Str_Choise, 1, 10 ), e , cd );
         Delete( Str_Choise, 1, 10 );              Del_Space( Str_Choise );
         if Str_Choise[1]='-' then begin
                                Ab:=-1.0;    Delete( Str_Choise, 1, 1 )  end
                              else Ab:=1.0;
         Val( Copy( Str_Choise, 1, 3 ), deg, cd );
         Val( Copy( Str_Choise, 5, 2 ), min, cd );
         Val( Copy( Str_Choise, 8, 4 ), sec, cd );   Delete( Str_Choise, 1, 11 );
         i:=Ab*pi/180.0*( deg*1.0+min/60.0+sec/3600.0 );
            Del_Space( Str_Choise );
         if Str_Choise[1]='-' then begin
                                Ab:=-1.0;    Delete( Str_Choise, 1, 1 )  end
                              else Ab:=1.0;
         Val( Copy( Str_Choise, 1, 3 ), deg, cd );
         Val( Copy( Str_Choise, 5, 2 ), min, cd );
         Val( Copy( Str_Choise, 8, 4 ), sec, cd );   Delete( Str_Choise, 1, 11 );
         Om:=Ab*pi/180.0*( deg*1.0+min/60.0+sec/3600.0 );
            Del_Space( Str_Choise );
         if Str_Choise[1]='-' then begin
                                Ab:=-1.0;    Delete( Str_Choise, 1, 1 )  end
                              else Ab:=1.0;
         Val( Copy( Str_Choise, 1, 3 ), deg, cd );
         Val( Copy( Str_Choise, 5, 2 ), min, cd );
         Val( Copy( Str_Choise, 8, 4 ), sec, cd );   Delete( Str_Choise, 1, 11 );
         w:=Ab*pi/180.0*( deg*1.0+min/60.0+sec/3600.0 );
            Del_Space( Str_Choise );
         Val( Copy( Str_Choise, 1, 5 ), tp, cd )
      end
end;    {     of  Read_Data_Kepler    }

procedure Time_to_Str( var TimeSt: string; Tt: float );
var i: integer;
    a: float;
    st1: string[15];
begin
   TimeSt:='';
   i:=Round( Int( Tt/3600.0 ) );   str( i, st1 );
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

procedure Abs_to_Grinv( var Plase: Efm_P; Year: word; T: float );
var Tn, SinAlfa, CosAlfa, So, Alfa: float;
                    i, Counter, Na: word;
                            Pl_Old: Efm_P;
begin
   Na:=( Year-1990 )*365;     Counter:=0;
   repeat
      if ( Year div 4 ) = ( Year/4 ) then Inc( Counter );
      Dec( Year )
   until Year = 1990;
   for i:=Today.month to 12 do Na:=Na-Monthes[i];
   Na:=Na+Counter+Today.Day-1;
   Tn:=Na/36525;
   So:=99.6909833+36000.768925*Tn+38.70833e-5*Sqr( Tn );
   Alfa:=So*pi/180+Omz*T;
   CosAlfa:=Cos( Alfa );       SinAlfa:=Sin( Alfa );    Pl_Old:=Plase;
   with Plase do
      begin
         x:=CosAlfa*Pl_Old.x+SinAlfa*Pl_Old.y;
         y:=-SinAlfa*Pl_Old.x+CosAlfa*Pl_Old.y;
         z:=Pl_Old.z;
         Vx:=-Omz*( SinAlfa*Pl_Old.x-CosAlfa*Pl_Old.y )+
                                       CosAlfa*Pl_Old.Vx+SinAlfa*Pl_Old.Vy;
         Vy:=-Omz*( CosAlfa*Pl_Old.x+SinAlfa*Pl_Old.y )-
                                       SinAlfa*Pl_Old.Vx+CosAlfa*Pl_Old.Vy;
         Vz:=Pl_Old.Vz
      end
end;


procedure Cga_Drv;  external;
procedure egavga_drv; external;

procedure Pause;
const mass:array[1..32] of word=( 1319, 1047, 1319, 1047, 1397, 1175, 1397, 1175, 659, 831,
                                 988, 1175, 1397, 1319, 1047, 880, 1047, 880, 1047, 880,
                                 1175, 880, 1175, 880, 659, 831, 988, 1175, 1175, 1047,
                                 880, 659 );
var   d: char;
      i: byte;
begin
   Info( '                   Press any key . . . ' );
   i:=1;
   repeat
      sound( mass[i] );      delay( 500 );
      i:=i+1;                if i=33 then i:=1
   until keypressed;
   nosound;                  d:=readkey
end;

procedure RK_Int( Y0: Efm_P; var Y: Efm_P; Step: float );
var Count1, ik: longint;

procedure RK_Sat_OneStep( Y0: Efm_P; var Y: Efm_P; Step: float );
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
   Ro:=6.367e-12*Exp( -0.954e-5*( Hight-H0 ) )
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
end;     {    RK_Sat_OneStep    }

begin    {    RK_Int   }
   Count1:=Round( Step/RK_Step );      Step:=RK_Step;
   for ik:=1 to Count1 do
      begin
         RK_Sat_OneStep( Y0, Y, Step );
         Y0:=Y
      end;
end;

procedure Invalid;
begin
   writeln(' Invalid parametrs . . .');
   writeln(' Usege TR_NES.EXE [-switch] ');
   writeln('   <Switches>');
   writeln('       w: Execution for NES-ephemerides ');
   writeln('       ?: Help.')
end;

procedure Status_Line( s: String );
var x, y : byte ;
   begin
      x:=WhereX;                                     y:=WhereY;
      Win( 1, 1, 80, 1, LightGray, LightCyan );      ClrScr;
      Write( s );
      Win( 1, 2, 80, 25, Bc, Tc );                   GotoXY( x, y );
      Kur( Off )
   end;


    { ***********************    MAIN   **************************** }


begin
   Smap:=NES;
   if (ParamStr( 1 ) = '-?') or (ParamStr( 1 ) = '/?') then
      begin
         Invalid;   writeln;    Exit
      end;
   Bc:=Blue;                    Tc:=Yellow;
   DetectGraph( Gd, Gm );
   if ( ( Gd = EGA ) or ( Gd = VGA ) ) then i:=RegisterBGIDriver( @egavga_drv )
                                 else i:=RegisterBGIDriver( @cga_drv );
   initgraph( Gd, Gm, '' );
   maxX:=GetMaxX;                   maxY:=GetMaxY;
   RestoreCrtMode;
   Win( 1, 1, 80, 25, Bc, Tc ); clrscr;
   Status_Line( SL_Mes );
if ParamCount=0 then
 begin
Start:
   NC:=0;
   Read_Data_Kepler( Sourse, Data, NC );     Sum_Days:=0;
   clrscr;
   assign( f1, 'date.$$$' );
{$I-}
   reset( f1 );
{$I+}
   IOold:=IOResult;
   if IOold = 0 then
      begin
         Readln( f1, DateSt );           Readln( f1, Time1 );
         Readln( f1, Time2 );            Readln( f1, Step );       close( f1 );
      end
       else
      begin
         DateSt:='dd mm yy';     Time1:='hh mm ss';
         Step:=30;               Time2:=Time1
      end;
   write( ' Date for prognosis: ' );
   Read3Num( Today.day, Today.month, Today.year, DateSt );
   Year_kep:=Century+Data.year;
   Year_now:=Century+Today.year;
   Sum_Days:=SD( Data, Today );
   writeln( '   Input time interval:' );
   write( '     begin: ..............' );  Read3Num( hh, mm, ss, Time1 );
         tk1:=3600.0*hh+60.0*mm+ss;
   write( '       end: ..............' );  Read3Num( hh, mm, ss, Time2 );
         tk2:=3600.0*hh+60.0*mm+ss;
   write( '   Step => 2 sec: ......' );    Read_Num( Step );
         rewrite( f1 );
         writeln( f1, DateSt );    writeln( f1, Time1 );   writeln( f1, Time2 );
         writeln( f1, Step );      writeln( f1, NC );
         close( f1 );
   Time:=Sum_Days*86400.0+tk1;
   Count:=Trunc( ( tk2-tk1 )/Step );                 InpF:='nes.out';
   Success:=False;
   repeat
      ClrScr;               Write( ' Path to data file : ' );
      Read_Str( path );     write( ' Output  file name : ' );
      Read_Str( InpF );     OpenInpFile( path+InpF, Success, 'o' )
   until Success;
   clrscr;
   writeln( f, '  Satellites ephemerides for Date: '+ DateSt );
   writeln( f, '  Grinvich coordinate.' );
   writeln( f );
   writeln( f, '  Time interval: ' + Time1 + ' - ' + Time2 + '. Step ',
                     Round( Step ), ' sec.');
   writeln( f );
   writeln( f, '  Time    Vx, [m/s]  Vy, [m/s]  Vz, [m/s]   '+
                  '   x, [m]       y, [m]       z, [m]' );
   write( '    Draving Map? (Y/N) ' );
   Kur( Off );                        c:=ReadKey;
   if UpCase( c ) = 'N' then Visible:=False else Visible:=True;
   if Visible then
      begin
         SetGraphMode( Gm );
         MAP( Smap );
         if Gd=CGA then SetColor( White ) else SetColor( LightMagenta );
         Rectangle( 437, maxY-35, 608, maxY-1 );
         Line( 437, maxY-26, 608, maxY-26 );
         if Gd = CGA then SetColor( White ) else SetColor( Yellow );
         OuttextXY( 527, maxY-23, Time1 );
         OuttextXY( 527, maxY-11, Time2 );
         OutTextXY( 154, maxY-33, DateSt );
         OutTextXY( 190, maxY-11, 'Single' );
         OuttextXY( 457, maxY-23, 'Begin - ' );
         OuttextXY( 457, maxY-11, ' End  - ' );
         OutTextXY( 507, maxY-34, 'WORK' )
      end;
   Tcr:=0.0;
   for i:=0 to Count do
      begin
         Trase_Kepler( Sourse, Time+i*Step, Point );
         if ( tk1+i*Step-Tcr ) > 86400.0 then Tcr:=Tcr+86400.0;
         Time_to_Str( TimeSt, tk1+i*Step-Tcr );
         Point_Gr:=Point;
         Abs_to_Grinv( Point_Gr, Year_now, tk1+i*Step );
         Trans_to_Geodezik( Point_Gr, Plase_Sat );
         with Point_Gr do writeln( f, TimeSt, Vx:11:4, Vy:11:4, Vz:11:4,
                                  x:13:1,  y:13:1,  z:13:1 );
         if Visible then
            begin
               SetFillStyle( 0, 0 );
               Bar( 151, maxY-23, 216, maxY-16 );
               SetFillStyle( 1, 1 );
               OutTextXY( 154, maxY-23, TimeSt );
               with Plase_Sat do
                  begin
                     L0:=( L.deg+L.min/60.0+L.sec/3600.0 );
                     if ( L0 > 180 ) then L0:=L0-360.0;
                     B0:=( B.deg+B.min/60.0+B.sec/3600.0 );
                  end;
               x:=Round( x0+cx*L0 );          y:=Round( y0+cy*B0 );
               PieSlice( x, y, 0, 359, 5 )
            end;
         if Visible and KeyPressed then
            begin
               ch:=ReadKey;
               Case UpCase( ch ) of
                    'E': begin
                            RestoreCrtMode;
                            Status_Line( SL_Mes );
                            Goto Start
                         end;
                    'S': begin
                            InfoMap( ' ENTER - continue ' );
                            readln;     InfoMap( ' E-exit / S-stop ' )
                         end
                  end
            end
      end;
   close( f );
   if Visible then
      begin
         InfoMap( ' Press any key . . . ' );    c:=ReadKey;
         RestoreCrtMode;                        Status_Line( SL_Mes )
      end;
   window( 1, 2, 80, 25 );
   TextColor( LightRed+Blink );   clrscr;
   write( '   E' );            TextColor( Tc );
   write( 'xecute for NavSS-ephemerides or continue ' );
   TextColor( LightRed+Blink );
   write( ' V' );            TextColor( Tc );   write( 'iew: ' );
   Kur( Off );               c:=ReadKey;
   if UpCase( c ) = 'V' then Goto Start else
              begin
                 Win( 1, 2, 80, 25, Black, White );     ClrScr;
                 Exit
              end
 end;


               {    Running NES - ephemerides . . . }


   if (ParamStr(1) = '-w') or (ParamStr(1) = '/w') then else
      begin
         Invalid;    writeln;    exit
      end;
   clrscr;
   if ParamCount = 2 then
         Val( Copy( ParamStr(2), 1, length(ParamStr(2)) ), NC, cd )
            else NC:=0;
   assign( f1, 'date.$$$' );
{$I-}
   reset( f1 );
{$I+}
   IOold:=IOResult;
   if IOold = 0 then
      begin
         Readln( f1, DateSt );           Readln( f1, Time1 );
         Readln( f1, Time2 );            Readln( f1, Step );
         close( f1 )
      end
       else
      begin
         DateSt:='dd mm yy';     Time1:='hh mm ss';   Step:=30;
         Time2:=Time1;
         write( ' Date for prognosis: ' );
         Read3Num( Today.day, Today.month, Today.year, DateSt );
         writeln( ' Input time interval:' );
         write( ' begin: ..............' );
                            Read3Num( hh, mm, ss, Time1 );
         tk1:=3600.0*hh+60.0*mm+ss;
                            write( '   end: ..............' );
         Read3Num( hh, mm, ss, Time2 );
         tk2:=3600.0*hh+60.0*mm+ss;
         write( ' Step => 2 sec: ......' );     Read_Num( Step );
      end;
   Read_Data_Kepler( Sourse, Data, NC );        Nvs:=0;  Sum_Days:=0;
   clrscr;
   with Today do
      begin
         Val( Copy( DateSt, 1, 2 ), day, cd );
         Val( Copy( DateSt, 4, 2 ), month, cd );
         Val( Copy( DateSt, 7, 2 ), year, cd )
      end;
   Year_kep:=Century+Data.year;
   Year_now:=Century+Today.year;
   Sum_Days:=SD( Data, Today );

   Val( Copy( Time1, 1, 2 ), hh, cd );
   Val( Copy( Time1, 4, 2 ), mm, cd );
   Val( Copy( Time1, 7, 2 ), ss, cd );
      tk1:=3600.0*hh+60.0*mm+ss;
   Val( Copy( Time2, 1, 2 ), hh, cd );
   Val( Copy( Time2, 4, 2 ), mm, cd );
   Val( Copy( Time2, 7, 2 ), ss, cd );
      tk2:=3600.0*hh+60.0*mm+ss;

   Time:=Sum_Days*86400.0+tk1;
   Count:=Trunc( ( tk2-tk1 )/Step );                 InpF:='nes.out';
   write( 'Output File name for ephemerides : ' );
   InpF:='nes.efm';                              Read_Str( InpF );
   assign( f, Path+InpF );                          rewrite( f );
   writeln( f, '  Satellites ephemerid for date: '+
                DateSt + '.' );
   writeln( f, '  Grinvich coordinate.' );
   writeln( f );
   write( f, '  Time interval:  ', Time1, ' - ', Time2, '.' );
   writeln( f, '  Step ', Round( Step ), ' sec.' );
   writeln( f );
   writeln( f, '  Time    Vx, [m/s]  Vy, [m/s]  Vz, [m/s]   '+
                  '   x, [m]       y, [m]       z, [m]' );
   Trase_Kepler( Sourse, Time, Point );
   Point_efm:=Point;
   Abs_to_Grinv( Point_efm, Year_now, tk1 );
   Time_to_Str( TimeSt, tk1 );
   Point:=Point_efm;
   with Point_efm do writeln( f, TimeSt, Vx:11:4, Vy:11:4, Vz:11:4,
                                             x:13:1,  y:13:1,  z:13:1 );
   ClrScr;
   Mulk_Yes( 26, 6, 40, 12, Cyan, Black );
   Window( 29, 10, 51, 15 );                       Writeln;
   Writeln( 'Runhe-Kutt Integration' );
   Writeln;
   Write( '   Processed  00  %' );   Tcr:=0.0;
   Kur( Off );
   for i:=1 to Count do
      begin
         RK_Int( Point, Point_efm, Step );
         Point:=Point_efm;
         if ( tk1+i*Step-Tcr ) > 86400.0 then Tcr:=Tcr+86400.0;
         Time_to_Str( TimeSt, tk1+i*Step-Tcr );
         GotoXY( 14, 4 );    Write( Round( 100.0*i/Count ):3 );
         Kur( Off );
         with Point_efm do writeln( f, TimeSt, Vx:11:4, Vy:11:4, Vz:11:4,
                                             x:13:1,  y:13:1,  z:13:1 );
      end;
   Mulk_No( 26, 6, 40, 12, Cyan, Black );
   close( f );
   closegraph;
   Pause;
   Win( 1, 1, 80, 25, Black, White );     ClrScr;
end.
