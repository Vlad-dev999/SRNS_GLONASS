{$L cga.obj}
{$L egavga.obj}
{$V-}
program TR_NSS;          {   Trace of Navigation Space Satellites    }
uses Graph, Crt, Dos;

label Start, Ending, Cont;

const Npoint = 1000;
       N_SAT = 30;
  BEAM_Angle = 19.0;
      SL_Mes = '    Trass Navigation Space Satellites by Almanah. ' +
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

    ar_400     = array[1..65520] of byte;
    ar_400_ptr = ^ar_400;

     Almanah_par = Record
           tOm: float;
             T: float;
             e: float;
             i: float;
            Om: float;
           arg: float;
            Tv: float
         end;

     W_Sat = Record
               N: byte;       { Номер рабочего спутника }
          Tb, Te: float       { Время начала и конца видимости }
        end;

     Efm_P_Ptr = ^Efm_P;
        Sat_ar = array[1..Npoint] of Efm_P_Ptr;
        ar_max = array[1..65520] of byte;
     Ptr_armax = ^ar_max;

const
    Monthes: array [1..12] of byte=( 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 );
     Omz = 0.7292115147*1E-4;      { [рад/c] }
      Sb = 1E-3;
      H0 = 9.0e+5;                 { [m] }
     C20 = -1082.637e-6;
      mz = 0.3986012*1E+15;   { Гравитационный потенциал Земли, [м3/с2] }
     ez2 = 6.6934216*1E-3;    { Квадрат Земного эксцентриситета }
      az = 6378136.0;         { Экваториальный радиус Земли, [м] }
      Ti = 8;
 Century = 1900;
    Angle_Vis = -65.0;
    RKGS_Step = 1.0;           { Step for RKGS-Subroutine, [c] }

var
 TimeSt, Nstr, Time1, Time2, Time3, ActAlmDateStr, Dat_Pro_str: string[10];
                        Alm_Date, InpF1, InpF, SStr, Outp_File: string[79];
                                                      Path, Pa: PathStr;
                                                          Name: NameStr;
                                                           Dir: DirStr;
                                                           Ext: ExtStr;
              IOold, y, Gm, Count, j, x, deg, min, Date_Alm, k: integer;
                                             x0, y0, i, Gd, cd: integer;
              L_Work, kk, N_Work, ik, Sat_Num, Nvs, hh, mm, ss: byte;
                                        NWSat, NChoise, Tc, Bc: byte;
                   MaxX, MaxY, Sat_C, Stack_Count, N, Year_Now: word;
                                                      Sum_Days: longint;
          Tcr, ab, TVbeg, TVend, Tbeg, Tend, sec, NTbeg, NTend: float;
                           cx, cy, Step, tk1, tk2, Bet, L0, B0: float;
                                       CMode, Success, Visible: boolean;
                                              Plase_Sat, Plase: Point_Country;
                                               Point_Gr, Point: Efm_P;
                                                Today, Dat_Pro: Date;
                                                       DirInfo: SearchRec;
                                                        ch1, c: char;
                                                         f, f1: text;
                                                          Smap: ForMAP;
                                                           OKA: Sat_Ar;
                               Glonass_Sat: array[1..N_SAT] of Almanah_Par;
                                  Work_Sat: array[1..150] of W_Sat;
                                   All_Sat: array[1..N_SAT] of W_Sat;
                                    Ar_Str: array[0..50] of Point_Str;
                                     Stack: array[1..100] of float;


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
    Col:=White;
    if Gd=CGA then SetColor( White ) else
       if CMode then SetColor( Yellow );
    Rectangle( 0, 0, maxX, maxY );
    Rectangle( 28, 0, maxX-28, maxY-47 );
    x0:=maxX div 2;                 y0:=( MaxY-47 ) div 2;
    cx:=( maxX-57 )/360.0;          cy:=-( MaxY-46 )/180.0;
    k:=-180;
    if Gd=CGA then SetColor( White ) else if CMode then SetColor( Green );
    if Gd=CGA then Col:=White else if CMode then Col:=Green;
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
    if Gd=CGA then SetColor( White ) else if CMode then SetColor( LightCyan );
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
    if Gd=CGA then SetColor( White ) else if CMode then SetColor( LightRed );
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
                  if CMode then SetColor( Red );     OuttextXY( 252, maxY-17, '*' );
                  if CMode then SetColor( Yellow );  OuttextXY( 258, maxY-17, ' - NES' )
               end
       end;
    if Gd = CGA then SetColor( White ) else if CMode then SetColor( Yellow );
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

procedure Choise( NChoise: byte; var Str_Choise: string );
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
                              Exit:=True;    Menu( j , Select );   Delay( 200 )
                           end
               end
         until Exit
      end;      {   if i > 1   }
   Str_Choise:=( Ar_Str[j] )^;
   for i:=0 to Nchoise do Dispose( Ar_Str[i] )
end;      {   of  Choise    }

procedure Read_Data_Kepler( var Sourse: Kepler_P; var Date_Kep: Date );
var
    Str_Choise: string;
  deg, min, cd: integer;
       Ab, sec: float;

begin
   ReadKeplerPrm;
   clrscr;
   Info( '   Choose working user ( keys: Up, Dn-move; ENTER-choose )' );
   Choise( NChoise, Str_Choise );
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

procedure Push( x: float );
begin
   Inc( Stack_Count );        Stack[Stack_Count]:=x
end;

function Pop: float;
begin
   Pop:=Stack[Stack_Count];   Dec( Stack_Count )
end;

function Angle_Plase( var Plase: Point_Country; var Sourse: Efm_P ): float;
var
     SinB0, CosB0, CosL0, SinL0, SinB02, sqsq, x0, y0, z0: float;
                                   delta, a2, b2, c2, Gam: float;
begin
   with Plase do
      begin
         SinB0:=Sin( Trans_to_Rad( B ) );     SinB02:=Sqr( SinB0 );
         CosB0:=Cos( Trans_to_Rad( B ) );     SinL0:=Sin( Trans_to_Rad( L ) );
         CosL0:=Cos( Trans_to_Rad( L ) );     sqsq:=az/Sqrt( 1.0-ez2*SinB02 );
         x0:=( sqsq+H )*CosL0*CosB0;
         y0:=( sqsq+H )*SinL0*CosB0;
         z0:=( sqsq*( 1.0-ez2 )+H )*SinB0
      end;
   with Sourse do
      begin
         a2:=x0*x0+y0*y0+z0*z0;         c2:=x*x+y*y+z*z;
         b2:=Sqr( x-x0 )+Sqr( y-y0 )+Sqr( z-z0 );
         Gam:=ArcCos( ( a2+b2-c2 )/2/Sqrt( a2*b2 ) );
      end;
   delta:=ArcCos( ( b2+c2-a2 )/2/Sqrt( c2*b2 ) );
   if delta > BEAM_Angle then Angle_Plase:=-90.0 else Angle_Plase:=Gam-90.0
end;

procedure RKGS( Y0: Efm_P; var Y: Efm_P; Step: float );
var Count1, ik: longint;

procedure RKG_Sat_OneStep( Y0: Efm_P; var Y: Efm_P; Step: float );
type
        ar_6 = array[1..6] of float;
      ar_4_6 = array[1..4] of ar_6;
var
   sqsq, r, V, r2, r3, Omz2, kol, fn, h:  float;
                                   i, j:  integer;
                                      K:  ar_4_6;

function Koeff( i: integer; var K: ar_4_6 ): float;
begin
   Koeff:=( K[1][i]+2.0*( K[2][i]+K[3][i] )+K[4][i] )/6.0
end;

procedure Inc_Point( var Y: Efm_P; var K: ar_6 );
begin
   with Y do
      begin
         Vx:=Y0.Vx+K[1]*kol;            x:=Y0.x+K[4]*kol;
         Vy:=Y0.Vy+K[2]*kol;            y:=Y0.y+K[5]*kol;
         Vz:=Y0.Vz+K[3]*kol;            z:=Y0.z+K[6]*kol
      end
end;

procedure SDE;
begin
   with Y do
      begin
         r:=Sqrt( x*x+y*y+z*z );
         V:=Sqrt( Vx*Vx+Vy*Vy+Vz*Vz );
         r2:=r*r;                  r3:=r*r2;
         sqsq:=( 1.0-5.0*z*z/r2 )*1.5*C20*mz/r2*Sqr( az )/r3-mz/r3;
      end;
   Omz2:=Sqr( Omz )
end;

begin
   h:=Step;        Y:=Y0;
   for j:=1 to 4 do for i:=1 to 6 do k[j][i]:=0;
   for j:=1 to 4 do
      begin
         SDE;
         case j of
               1: kol:=0.5;
               2: kol:=0.5;
               3: kol:=1.0;
               4: kol:=0.0
            end;
         for i:=1 to 6 do
            begin
               case i of
                     1: fn:=Y.x*(sqsq+Omz2)+2*Omz*Y.Vy;   {  F1( Y );  }
                     2: fn:=Y.y*(sqsq+Omz2)-2*Omz*Y.Vx;   {  F2( Y );  }
                     3: fn:=Y.z*sqsq;                     {  F3( Y );  }
                     4: fn:=Y.Vx;                         {  F4( Y );  }
                     5: fn:=Y.Vy;                         {  F5( Y );  }
                     6: fn:=Y.Vz;                         {  F6( Y )   }
                  end;
               K[j][i]:=h*fn
            end;
         case j of
               1, 2, 3: Inc_Point( Y, K[j] )
            end
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
   Count1:=Round( Step/RKGS_Step );      Step:=RKGS_Step;
   for ik:=1 to Count1 do
      begin
         RKG_Sat_OneStep( Y0, Y, Step );
         Y0:=Y
      end
end;

function Alm_Par_Comparation( Par: Almanah_Par ): Boolean;
begin
   with Par do
         if ( tOm=0.0 ) and ( T=0.0 ) and ( e=0.0 ) and ( i=0.0 ) and
                        ( Om=0.0 ) and ( Tv=0.0 ) and ( arg=0.0 ) then
            Alm_Par_Comparation:=True else Alm_Par_Comparation:=False
end;

procedure Null_Par( Par: Ptr_armax; N: word );
begin
   for i:=1 to N do Par^[i]:=0
end;

procedure CGA_Drv;  external;

procedure AvrExit ;
   begin
      AvrSound;    TextBackground( 0 );    ClrScr;    Halt( 1 )
   end;

procedure Trass_NKA_Alm( Sourse: Almanah_par; Time: float; var Point: Efm_P );
var
      Mk, Ek, CosEk, SinEk, Znam, rk, CosVk, SinVk, SinW, CosW:  float;
      A, dt, Tt, ddd, SinUk, CosUk, Coslam, Sinlam, Cosi, Sini:  float;
    tp, nn, w, P, rv, dlam, lam, dw, Vlam, Elam, drk, dVk, drr:  float;
                                                             N:  integer;

function Tan( a: float ): float;
begin
   Tan:=Sin( a )/Cos( a )
end;

function RelStep( m, n: float ): float;
  begin
    relstep:=exp( n*ln( m ) )
  end;

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

begin    {      Trass_NKA_Alm     }
   N:=Sum_days;
   if Date_Alm > N then
      begin
         Info( '       Forecast Imposible.  Press any key . . .' );
         Delay( 2000 );                    AvrExit
      end;
   dt:=( N-Date_Alm )*86400.0+Time;
   with Sourse do
      begin
         Tt:=T+Tv/T*( dt-tOm );       Cosi:=Cos( i );      nn:=2*pi/Tt;
         A:=RelStep( mz/nn/nn, 1.0/3.0 );
         ddd:=3*pi/Tt*Sqr( az/A/( 1.0-ez2 ) );
         dlam:=-ddd*Cosi*C20;
         dw:=ddd/2*C20*( 5*Sqr( Cosi )-1.0 );
         lam:=Om+( dlam-Omz )*( dt-tOm );
         lam:=2*pi*Frac( lam/pi/2 );
         w:=arg+dw*( dt-tOm );
         Vlam:=2*pi-w;
         Elam:=2.0*ArcTan( Sqrt( ( 1.0-e )/( 1.0+e ) )*Tan( 0.5*Vlam ) );
         tp:=tOm-( ( Elam-e*Sin( Elam ) ) )/nn;
         Mk:=nn*( dt-tp );
         Mk:=2*pi*Frac( Mk/pi/2 );
         Ek:=Kepler_Eq_MPP( e, Mk );
         CosEk:=Cos( Ek );               SinEk:=Sin( Ek );
         Znam:=1.0-e*CosEk;              rk:=a*Znam;
         CosVk:=( CosEk-e )/Znam;        SinVk:=Sqrt( 1-Sqr( e ) )*SinEk/Znam;
         SinW:=Sin( w );                 CosW:=Cos( w );
         SinUk:=SinW*CosVk+CosW*SinVk;
         CosUk:=CosW*CosVk-SinW*SinVk;
         Coslam:=Cos( lam );             Sinlam:=Sin( lam );
         Cosi:=Cos( i );                 Sini:=Sin( i );
         P:=A*( 1.0-e*e );
         drk:=Sqrt( mz/P )*e*SinVk;      dVk:=Sqrt( mz*P )/rk/rk
      end;
   with Point do
      begin
         x:=rk*( CosUk*Coslam-SinUk*Sinlam*Cosi );
         y:=rk*( CosUk*Sinlam+SinUk*Coslam*Cosi );
         z:=rk*SinUk*Sini;
         drr:=drk/rk;                  rv:=rk*dVk;
         Vx:=drr*x-rv*( SinUk*Coslam+CosUk*Sinlam*Cosi )+Omz*y;
         Vy:=drr*y-rv*( SinUk*Sinlam-CosUk*Coslam*Cosi )-Omz*x;
         Vz:=drr*z+rv*CosUk*Sini
      end
end;      {   Trass_NKA_Alm    }

procedure Choise_Alm( NChoise: byte; var Str_Choise: string );
var i, kmax, xx, yy, j, k, xcur, ycur: byte;
                                 Exit: boolean;
                                    c: char;
procedure Menu ( nMelm : byte ; State : StateType ) ;
var Bc, Tc : byte ;
begin
   case State of
         Passive : begin Bc:=1;           Tc:=LightGreen   end;
         Select  : begin Bc:=Green;       Tc:=LightRed     end;
         Active  : begin Bc:=LightGray;   Tc:=Red          end
      end;
   Win( xx, yy+nMelm-1, xx+kmax+1, yy+nMelm-1, Bc, Tc );    ClrScr;
   Write( ' '+( Ar_Str[nMelm] )^ );
   Win( 1, 2, 80, 25, Bc, Tc );
   GotoXY( Xcur, Ycur );
   Kur( Off )
end;

begin    {   Choise   }
   Success:=False;
       repeat
          ClrScr;              Write( '  Path to Almanah files : ' );
          Read_Str( Path );    new( Ar_Str[0] );
          if Path <> '' then Path:=Path+'\';
          FindFirst( Path+'*.alm', Archive, DirInfo );
          if ( DosError<>0 ) and ( DosError<>18 ) then begin
             Info( 'Path not found . . . Press any key' );
             end;
          if DosError=18 then begin
             Info( 'Almanah files not exist here . . . '+
                  'Press any key' );    AvrSound end;
          j:=1;
          while ( DosError=0 ) and ( j<=20 ) do begin
             Success:=True; New( Ar_Str[j] );
             Ar_str[j]^:=DirInfo.Name;
             FindNext( DirInfo );
             if DosError=0 then Inc( j ) end
       until Success;
  ClrScr;  NChoise:=j;
  Info( '   Choose Almanah file ( keys: Up, Dn-move; '+
                       'ENTER-choose )' );
  Ar_Str[0]^:='Date of Almanah  ';
  kmax:=1;      Xcur:=WhereX;         Ycur:=WhereY;       i:=1;
  while ( i <= NChoise ) and ( Ar_Str[i]^ <> '' ) do
     begin
         k:=Length( Ar_Str[i]^ );
         if k > 78 then kmax:=78 else
            if k > kmax then kmax:=k;
         Inc( i )
      end;
      j:=1;
   if i > 1 then
      begin
         xx:=40-( kmax div 2 );        yy:=13-( NChoise div 2 );
         GotoXY( xx-1, yy-2 );         Write( Ar_Str[0]^ );
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
                              Exit:=True;    Menu( j, Select );  Delay( 200 )
                           end
               end
         until Exit
      end;      {   if i > 1   }
   Str_Choise:=Ar_Str[j]^;
   for i:=0 to Nchoise do Dispose( Ar_Str[i] )
end;              {   of  Choise_Alm   }

procedure egavga_drv; external;

procedure Status_Line( s: string );
var x, y : byte ;
   begin
      x:=WhereX;                                     y:=WhereY;
      Win( 1, 1, 80, 1, LightGray, LightCyan );      ClrScr;
      Write( s );
      Win( 1, 2, 80, 25, Bc, Tc );                   GotoXY( x, y );
      Kur( Off )
   end;

procedure MNul( A: ar_400_ptr; n: word );
var i: word;
begin
   for i:=1 to n do A^[i]:=0
end;

procedure Visible_Map;
const StartX = 40;
        Hour = 3600;
    HalfHour = 1800;
     SixHour = 600;
var      x2, y2, x1, y1, StartY, endX, endY: word;
                    sh, hh, h, TimeStep, tt: float;
                                 TStr, Sdig: string[15];
                                       Mess: String;
                                        col: byte;
                                          i: longint;
begin
   SetColor( White );                      Col:=White;
   StartY:=maxY-30;                        if CMode then SetColor( Yellow );
   Rectangle( 0, 0, MaxX, maxY );          Rectangle( 1, 1, MaxX-1, maxY-1 );
   line( 0, 16, maxX, 16 );                line( 0, maxY-16, maxX, maxY-16 );
   tt:=tk2-tk1;                            if CMode then SetColor( Green );
   line( StartX, StartY, maxX-10, StartY );
   line( maxX-10, StartY, maxX-10-( maxX div 27 ), StartY-(maxY div 120) );
   line( maxX-10, StartY, maxX-10-( maxX div 27 ), StartY+(maxY div 120) );
   line( StartX, StartY, StartX, 25 );
   line( StartX, 25, StartX-( maxX div 200 ), 25+(maxY div 20) );
   line( StartX, 25, StartX+( maxX div 200 ), 25+(maxY div 20) );
   if CMode then SetColor( LightGreen );
   SetTextJustify( LeftText, BottomText );
   OutTextXY( 5, StartY+12, Time1[1]+Time1[2]+' '+Time1[4]+Time1[5]+'''' );
   OutTextXY( MaxX-48, StartY+12, Time2[1]+Time2[2]+' '+Time2[4]+Time2[5]+'''' );
   SetTextJustify( CenterText, CenterText );
   OutTextXY( StartX div 2, 22, 'Sat' );
   OutTextXY( maxX-15, StartY-10, 'T,c' );
   endX:=maxX-StartX-30+1;           TimeStep:=tt/endX;
   for i:=1 to N_SAT do
      begin
         y1:=StartY-Round( (maxY-65)/N_SAT*i );    x1:=StartX+2;
         Str( i, Sdig );
         if (i div 2) <> (i/2) then OutTextXY( (x1 div 2)-10, y1, Sdig )
               else OutTextXY( (x1 div 2)+7, y1, Sdig );
         line( x1-4, y1, x1, y1 );
         if CMode then if Gd <> CGA then col:=LightRed else col:=White;
         while x1 < (maxX-30) do
            begin
               PutPixel( x1, y1, col );    inc( x1, 6 )
            end
      end;
   sh:=SixHour;      hh:=HalfHour;    h:=Hour;
   for i:=1 to endX do
      begin
         if CMode then if Gd <> CGA then col:=LightRed else col:=White;
         if TimeStep*i >= Sh then
            begin
               sh:=sh+SixHour;
               Line( StartX+i, StartY, StartX+i, StartY-(maxY div 100) );
            end;
         if TimeStep*i >= hh then
            begin
               y1:=StartY;
               while (y1 > 40) and (tt <= 18000.0) do
                  begin
                     PutPixel( StartX+i, y1, col );    Dec( y1, 8 )
                  end;
               hh:=hh+HalfHour;
               Line( StartX+i-1, StartY, StartX+i-1, StartY-(maxY div 100) );
               Line( StartX+i+1, StartY, StartX+i+1, StartY-(maxY div 100) )
            end;
         if TimeStep*i >= h then
            begin
               Time_To_Str( TStr, tk1+TimeStep*i );
               y1:=StartY;
               if TStr <> Time2 then
                     OutTextXY( StartX+i, StartY+10, TStr[1]+TStr[2] );
               while y1 > 40 do
                  begin
                     PutPixel( StartX+i, y1, col );    Dec( y1, 8 )
                  end;
               h:=h+Hour;    SetLineStyle( SolidLN, 0, ThickWidth );
               Line( StartX+i, StartY, StartX+i, StartY-(maxY div 50) );
               SetLineStyle( SolidLN, 0, NormWidth );
            end;
      end;
   for i:=1 to k do with Work_Sat[i] do
      begin
         if CMode then SetColor( White );
         SetLineStyle( SolidLN, 0, NormWidth );
         y1:=StartY-Round( (maxY-65)/N_SAT*N );
         x1:=StartX+Round( (Tb-tk1)/TimeStep );
         x2:=StartX+Round( (Te-tk1)/TimeStep );
         line( x1, y1, x2, y1 );
         if CMode then SetColor( LightBlue );
         SetLineStyle( UserBitLN, $1111, NormWidth );
         line( x1, y1, x1, StartY );
         line( x2, y1, x2, StartY );
      end;
   if CMode then SetColor( White );
   Mess:='Date: ' + Dat_Pro_Str + '     Alm. date: ' + Alm_Date;
   Str( NWSat, Sdig );
   Mess:=Mess + '    ' + Sdig + ' Sat. are present in almanah.';
   SetTextJustify( CenterText, CenterText );
   OuttextXY( maxX div 2, maxY-8, Mess );
   Mess:=' Visible Satellites map for work seanse. ';
   OuttextXY( maxX div 2, 9, Mess );
   if CMode then SetColor( Red );
   SetLineStyle( SolidLn, 0, ThickWidth );
   for i:=1 to Round( Stack_Count/2 ) do
      begin
         x1:=StartX+Round( (Stack[2*i-1]-tk1)/TimeStep );
         x2:=StartX+Round( (Stack[2*i]-tk1)/TimeStep );
         y1:=StartY-9;
         line( x1, y1, x2, y1 );
      end;
end;

                  {***********   MAIN   ***********}
begin
   Stack_Count:=0;                   Smap:=NSS;
   Null_Par( @Stack, SizeOf( Stack ) );
   DetectGraph( Gd, Gm );
   if ( ( Gd = EGA ) or ( Gd = VGA ) ) then i:=RegisterBGIDriver( @egavga_drv ) else
                                            i:=RegisterBGIDriver( @cga_drv );
   initgraph( Gd, Gm, '' );
   maxX:=GetMaxX;                 maxY:=GetMaxY;
   RestoreCrtMode;
   Status_Line( SL_Mes );
   ik:=0;
   Tc:=Yellow;                    Bc:=Blue;
   Win( 1, 2, 80, 25, Bc, Tc );   clrscr;
   Null_Par( @Work_Sat, SizeOf( Work_Sat ) );
   for i:=1 to N_SAT do All_Sat[i]:=Work_Sat[1];
   Null_Par( @Glonass_Sat, SizeOf( Glonass_Sat ) );
   Pa:=FSearch( 'tr_nss.exe', GetEnv( 'PATH' ) );
   FSplit( FExpand( Pa ), Dir, Name, Ext );
   path:=Dir+'DATA\';
   Choise_Alm( NChoise, ActAlmDateStr );
   Alm_Date:=ActAlmDateStr[2]+ActAlmDateStr[3]+'.'+ActAlmDateStr[4]+
             ActAlmDateStr[5]+'.'+ActAlmDateStr[6]+ActAlmDateStr[7];
   Success:=False;
   repeat
      OpenInpFile( Path+ActAlmDateStr, Success, 'i' )
   until Success;
   i:=1;
   readln( f, Sstr );
   while Sstr[i] <> ' ' do Inc( i );            Dec( i );
   Val( Copy( Sstr, 1, i ), Date_Alm, cd );
   readln( f );         Clrscr;
   Sstr:='';
   while Sstr='' do readln( f, Sstr );
   Info( '   Reading  almanah  from  '+Path+'  file . . .' );
   while not Eof( f ) do
      begin
         readln( f, Sstr );
         Del_Space( Sstr );     i:=1;
         while Sstr[i] <> ' ' do Inc( i );     Dec( i );
         Val( Copy( Sstr, 1, i ), Sat_Num, cd );
         with Glonass_Sat[Sat_Num] do
            begin
               while Sstr[1] <> ' ' do Delete( Sstr, 1, 1 );
               Del_Space( Sstr );                      j:=1;
               while Sstr[j] <> ' ' do Inc( j );       Dec( j );
               Val( Copy( Sstr, 1, j ), tOm, cd );
               Delete( Sstr, 1, j );     Del_Space( Sstr );
               Val( Copy( Sstr, 1, 9 ), T, cd );
               Delete( Sstr, 1, 9 );     Del_Space( Sstr );
               Val( Copy( Sstr, 1, 10 ), e, cd );
               Delete( Sstr, 1, 10 );    Del_Space( Sstr );
               Val( Copy( SStr, 1, 2 ), deg, cd );
               Val( Copy( SStr, 4, 2 ), min, cd );
               Val( Copy( SStr, 7, 4 ), sec, cd );     Delete( SStr, 1, 10 );
               i:=pi/180.0*( deg*1.0+min/60.0+sec/3600.0 );
               Del_Space( SStr );
               Val( Copy( SStr, 1, 3 ), deg, cd );
               Val( Copy( SStr, 5, 2 ), min, cd );
               Val( Copy( SStr, 8, 4 ), sec, cd );     Delete( SStr, 1, 11 );
               Om:=pi/180.0*( deg*1.0+min/60.0+sec/3600.0 );
               Del_Space( SStr );
               Val( Copy( SStr, 1, 3 ), deg, cd );
               Val( Copy( SStr, 5, 2 ), min, cd );
               Val( Copy( SStr, 8, 4 ), sec, cd );   Delete( SStr, 1, 11 );
               arg:=pi/180.0*( deg*1.0+min/60.0+sec/3600.0 );
               Del_Space( SStr );
               if SStr[1]='-' then
                  begin
                     ab:=-1.0;   Delete( SStr, 1, 1 )
                  end
                        else ab:=1.0;
               Val( Copy( SStr, 1, 10 ), Tv, cd );   Tv:=Tv*ab;
            end
      end;
   close( f );
   InpF1:='nes.out';                    Kur( ON );
   write( '  Input file with near-earth satellite ephemerid`s: ' );
   Read_Str( InpF1 );
   Success:=False;
   repeat
      OpenInpFile( Path+InpF1, Success, 'i' )
   until Success;

   readln( f, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   Dat_Pro_Str:=Copy( Sstr, 1, 8 );
   with Dat_Pro do
      begin
         Val( Copy( Sstr, 1, 2 ), day, cd );
         Val( Copy( Sstr, 4, 2 ), month, cd );
         Val( Copy( Sstr, 7, 2 ), year, cd )
      end;
   Today:=Dat_Pro;
   while Sstr <> '' do readln( f, Sstr );
   while Sstr='' do readln( f, Sstr );
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
   Sstr:='';      while Sstr='' do Readln( f, Sstr );
   Val( Copy( Time1, 1, 2 ), hh, cd );
   Val( Copy( Time1, 4, 2 ), mm, cd );
   Val( Copy( Time1, 7, 2 ), ss, cd );
      tk1:=hh*3600.0+mm*60.0+ss*1.0;
   Val( Copy( Time2, 1, 2 ), hh, cd );
   Val( Copy( Time2, 4, 2 ), mm, cd );
   Val( Copy( Time2, 7, 2 ), ss, cd );
      tk2:=hh*3600.0+mm*60.0+ss*1.0;

   N:=Round( ( tk2-tk1 )/Step ) + 1;
   if MemAvail < SizeOf( Efm_P )*N then begin
               Info( '  No Free Memory. Program halted. Press any key . . . ' );
               AvrExit
            end
                else for i:=1 to N do New( OKA[i] );
   for i:=1 to N do
      begin
         readln( f, Sstr );
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
               while Sstr[k] <> ' ' do Inc( k );    Dec( k );
               Val( Copy( Sstr, 1, k ), z, cd )
            end;
      end;
   close( f );        NWSat:=0;
   for Sat_Num:=1 to N_SAT do
         if Alm_Par_Comparation( Glonass_Sat[Sat_Num] ) then else Inc( NWSat );


Start:
   ClrScr;      Visible:=True;
   Info( '  SPACE - modify;  ENTER - set screen mode. ' );
   repeat
      GotoXY( 1, 1);    clreol;
      write('    Drawing Map?  ');
      TextColor( White );
      if Visible then writeln('Yes') else Writeln(' No');
      Kur( Off );
      c:=Readkey;
      TextColor( Tc );
      if c = ' ' then Visible:=Not Visible;
   until c=#13;
   CMode:=True;
   repeat
      GotoXY( 1, 2 );    clreol;
      write('    Screen mode: ');
      TextColor( White );
      if CMode then writeln('Color') else Writeln('Black & White');
      Kur( Off );
      c:=Readkey;
      TextColor( Tc );
      if c = ' ' then CMode:=Not CMode;
   until c=#13;
   ClrScr;
   MNul( @Work_Sat, SizeOf(Work_Sat) );
   Sum_Days:=0;
   Year_Now:=1900+Dat_Pro.year;
   if ( ( Year_Now div 4 ) = ( Year_Now/4 ) ) and ( Dat_Pro.Month < 3 ) then else
                                                     Sum_days:=Sum_days+1;
   while ( Year_Now div 4 ) <> ( Year_Now/4 ) do Dec( Year_Now );
   Sum_Days:=Sum_days+( 1900+Dat_Pro.year-Year_Now )*365;
   for i:=1 to Dat_Pro.month-1 do Sum_days:=Sum_days+Monthes[i];
   Sum_days:=Sum_days+Dat_Pro.day-1;
   Count:=Trunc( ( tk2-tk1 )/Step );
   Tbeg:=tk1;     Tend:=tk2;
   if Visible then
      begin
         SetGraphMode( Gm );
         MAP( Smap );
         if Gd=CGA then SetColor( White ) else if CMode then SetColor( LightMagenta );
         Rectangle( 437, maxY-35, 608, maxY-1 );
         Line( 437, maxY-26, 608, maxY-26 );
         if Gd = CGA then SetColor( White ) else if CMode then SetColor( Yellow );
         OuttextXY( 527, maxY-23, Time1 );
         OuttextXY( 527, maxY-11, Time2 );
         OuttextXY( 457, maxY-23, 'Begin - ' );
         OuttextXY( 457, maxY-11, ' End  - ' );
         OutTextXY( 507, maxY-34, 'WORK' );
         OutTextXY( 154, maxY-33, Dat_Pro_Str )
      end
         else begin
                 clrscr;
                 Info( '    Wait please . . .' );
                 Mulk_Yes( 26, 6, 40, 12, Green, Black );
                 window(40-13, 12-3, 40+13, 12+3 );
                 gotoXY( 7, 3 ); write('Execution . . .');
                 gotoXY( 7 ,5 ); write('Processed  00  %');
                 Kur( Off )
              end;
   TVbeg:=0.0;    TVend:=0.0;      N_Work:=0;     ik:=0;     k:=0;
   NTbeg:=0.0;    NTend:=0.0;      Tcr:=0.0;
   for i:=0 to Count do
      begin
         if (tk1+i*Step-Tcr) > 86400.0 then Tcr:=Tcr+86400.0;
         Time_to_Str( TimeSt, tk1+i*Step-Tcr );
         Trans_to_Geodezik( OKA[i+1]^, Plase );
         if Visible then
            begin
               SetFillStyle( 0, 0 );
               Bar( 151, maxY-23, 216, maxY-16 );
               SetFillStyle( 1, 1 );
               OutTextXY( 154, maxY-23, TimeSt );
               with Plase do
                  begin
                     L0:=( L.deg+L.min/60.0+L.sec/3600.0 );
                     if ( L0 > 180 ) then L0:=L0-360.0;
                     B0:=( B.deg+B.min/60.0+B.sec/3600.0 );
                  end;
               if Gd=CGA then SetColor( White ) else if CMode then SetColor( Red );
               OutTextXY( Round(x0+cx*L0), Round(y0+cy*B0), '*' );
               if Gd=CGA then SetColor( White ) else if CMode then SetColor( Yellow )
            end
         else
            begin
               window(40-13, 12-3, 40+13, 12+3);
               gotoXY(17, 5 );    Write( Round(i*100.0/Count):3 )
            end;
         Success:=False;
         for Sat_Num:=1 to N_SAT do
            if Alm_Par_Comparation( Glonass_Sat[Sat_Num] ) then else
               begin
                  if KeyPressed and Visible then
                     begin
                        ch1:=ReadKey;
                        Case UpCase( ch1 ) of
                              'E': begin
                                      RestoreCrtMode;
                                      Status_Line( SL_Mes );
                                      Goto Start    end;
                              'S': begin   InfoMap( ' ENTER - continue ' );
                                           readln;
                                           InfoMap( ' E-exit / S-stop ' )
                                   end
                           end
                     end;
                  Trass_NKA_Alm( Glonass_Sat[Sat_Num], tk1+i*Step, Point );
                  Point_Gr:=Point;
                  Trans_to_Geodezik( Point_Gr, Plase_Sat );
                  Bet:=Angle_Plase( Plase, Point_Gr );
                  if Visible then
                     begin
                        with Plase_Sat do
                           begin
                              L0:=( L.deg+L.min/60.0+L.sec/3600.0 );
                              if ( L0 > 180 ) then L0:=L0-360.0;
                              B0:=( B.deg+B.min/60.0+B.sec/3600.0 )
                           end;
                        x:=Round( x0+cx*L0 );       y:=Round( y0+cy*B0 );
                     end;

                  if Bet > Angle_Vis then
                     begin
                        if Visible then PieSlice( x, y, 0, 360, 5 );
                        Inc( N_Work );   Success:=True
                     end
                            else  begin
                                     if Visible then Circle( x, y, 5 );
                                     Success:=False
                                  end;
                   if Success then with All_Sat[Sat_Num] do
                     begin
                        if Tb = 0.0 then
                           begin
                              Tb:=tk1+i*Step;     N:=Sat_Num
                           end;
                        Te:=tk1+i*Step;
                     end
                         else if All_Sat[Sat_Num].Tb <> 0.0 then
                            begin
                               Inc( k );     Work_Sat[k]:=All_Sat[Sat_Num];
                               with All_Sat[Sat_Num] do
                                  begin
                                     Tb:=0.0;    Te:=0.0;    N:=0
                                  end
                            end;
               end;
         if Visible then
            begin
               SetFillStyle( 0, 0 );          Bar( 216, maxY-13, 246, maxY-3 );
               Str( N_Work, Nstr );           OutTextXY( 225, maxY-11, Nstr );
               SetFillStyle( 1, 1 )
            end;
         if (N_Work > 3) and (TVbeg = 0.0) then TVbeg:=tk1+i*Step;
         if N_Work > 3 then TVend:=tk1+i*Step;
         if (N_Work < 4) and (TVbeg <> 0.0) then
            begin
               Push( TVbeg );        Push( TVend );
               NTbeg:=TVbeg;         NTend:=TVend;
               TVbeg:=0.0;           TVend:=0.0
            end;
         if N_Work > ik then ik:=N_Work;    L_Work:=N_Work;
         N_Work:=0
      end;
   if L_Work > 3 then
      begin
         NTbeg:=TVbeg;      NTend:=TVend;
         Push( TVbeg );     Push( TVend )
      end;
   for i:=1 to N_SAT do if All_Sat[i].tb <> 0.0 then
      begin
         Inc( k );     Work_Sat[k]:=All_Sat[i]
      end;
   if Visible then
      begin
         InfoMap( ' Press any key . . . ' );    c:=ReadKey;
         RestoreCrtMode;                        Status_Line( SL_Mes );
         window( 1, 2, 80, 25 )
      end
         else Mulk_No(26, 6, 40, 12, Green, Black );
   win( 1, 2, 80, 25, Bc, Tc );
   clrscr;
   write( '  File name for map of visible satellites: ' );
   InpF:='visible.map';             Read_Str( InpF );
   Success:=False;
   repeat
      OpenInpFile( Path+InpF, Success, 'o' )
   until Success;
   clrscr;
   writeln( f, '   Visible satellite map for date: '+ Dat_Pro_Str +
            '. ' + Time1 + ' - ' + Time2 + '. Step ', Round( Step ), '.' );
   writeln( f );
   writeln( f, 'Num', 'Time begin':15, 'Time end':15, 'Time int':15,
                     'Sat. Num ':15 );
   writeln( '   Visible satellite map for date: '+Dat_Pro_Str );
   writeln;
   writeln( 'Num', 'Time begin':15, 'Time end':15, 'Time int':15,
                     'Sat. Num ':15 );
   for Sat_Num:=1 to k do with Work_Sat[Sat_Num] do
            begin
               Time_to_Str( Time1, Tb );     Time_to_Str( Time2, Te );
               Time_to_Str( Time3, Te-Tb );
               writeln( f, Sat_Num:3, Time1:15, Time2:15, Time3:15, N:15 );
               writeln( Sat_Num:3, Time1:15, Time2:15, Time3:15, N:15 )
            end;
   close( f );
   Info( '  Press any key . . . ' );    Kur( Off );      c:=ReadKey;
   if ik < 4 then
      begin
         write('Work interval is empty. ' +
                    'Continue for new time interval ? (Y/N)');
         c:=ReadKey;
         if UpCase(c) = 'Y' then Halt( 100 ) else Goto Ending
      end;
   Time_to_Str( Time1, tk1 );    Time_to_Str( Time2, tk2 );
   SetGraphMode( Gm );
   Visible_Map;          c:=ReadKey;
   RestoreCrtMode;       Status_Line( SL_Mes );
   clrscr;

   j:=Stack_Count;
   for i:=1 to Round( j/2 ) do
      begin
         NTend:=Pop;                       NTbeg:=Pop;
         Time_To_Str( Time1, NTbeg );      Time_To_Str( Time2, NTend );
         writeln( '  Work interval is: ' + Time1 + ' - ' + Time2 + '.' )
      end;


   tk1:=NTbeg;    tk2:=NTend;
   Time_To_Str( Time3, NTend-NTbeg );
   writeln;                        writeln;
   writeln( '  Work time interval is: ...', Time3 );
   writeln( '  Begin.....................', Time1 );
   writeln( '  End.......................', Time2 );
   writeln;
   ClrInfo;
   write( '  Work at this time interval ? (Y/N) ' );         c:=ReadKey;
   writeln;
   Success:=False;
   repeat
      Case Upcase( c ) of
         'N': begin
                 writeln;
                 writeln( '  Input new time interval :' );     clreol;
                 write( '  Begin....................' );       clreol;
                 Read3Num( hh, mm, ss, Time1 );
                 Tk1:=3600.0*hh+60.0*mm+ss;
                 write( '  End......................' );       clreol;
                 Read3Num( hh, mm, ss, Time2 );
                 Tk2:=3600.0*hh+60.0*mm+ss;
                 for i:=1 to Round( j/2 ) do
                       Success:=Success or ((tk1 >= Stack[2*i-1]) and
                                            (tk2 <= Stack[2*i]));
                 if not Success then
                    begin
                       k:=WhereY;
                       Info( '   Invalid interval' );
                       Delay( 400 );          GotoXY( 1, k-4 );
                       clreol
                    end
              end
            else Success:=True;
         end
   until Success;
   writeln;
   write( '  Input step for ephemerides calculation => 2 sec: ' );
   Readln( Step );
   Outp_File:='glonass.efm';
   write( ' Output file name: ' );   Read_Str( Outp_File );
   Success:=False;
   repeat
      OpenInpFile( Path+Outp_File, Success, 'o' )
   until Success;
   clrscr;
   assign( f1, 'date.$$$' );
{$I-}
   reset( f1 );
{$I+}
   IOold:=IOResult;
   if IOold = 0 then
      begin
         Readln( f1 );       Readln( f1 );            Readln( f1 );
         Readln( f1 );       Readln( f1, NChoise );   close( f1 );
      end
          else NChoise:=0;
{$I-}
   rewrite( f1 );
{$I+}
   IOold:=IOResult;
   if IOold = 0 then
      begin
         writeln( f1, Dat_Pro_Str );   Writeln( f1, Time1 );
         Writeln( f1, Time2 );         Writeln( f1, Step );
         Writeln( f1, NChoise );       close( f1 );
      end
          else
             begin
                Info(' File date.$$$ - error. Press any key . . . ');
                AvrExit
             end;

   Count:=Round((tk2-tk1)/Step)+1;
   N_Work:=0;
   for i:=1 to k do with Work_Sat[i] do
         if (Tb < tk1) and (Te > tk2) or
            (Tb < tk1) and (Te > tk1) or
            (Tb < tk2) and (Te > tk2) or
            (Tb < tk2) and (Te > tk1) then
            begin
               if Tb < tk1 then Tb:=tk1;
               if Te > tk2 then Te:=tk2;
               Inc( N_Work);
               Work_Sat[N_Work]:=Work_Sat[i]
            end;

   writeln( f, N_Work:5, ' satellites. Grinvich coordinates.' );
   write( f, '  Time interval:  ', Time1, ' - ', Time2, '.' );
   writeln( f, '  Step ', Round( Step ), ' sec.' );
   writeln( f );
   writeln( f, '   N  Sat ':12, 'Tbeg     ':15, 'Tend      ':15);
   for i:=1 to N_Work do with Work_Sat[i] do
      begin
         Time_To_Str(Time1, Tb);         Time_To_Str(Time2, Te);
         writeln( f, N:10, Time1:15, Time2:15 )
      end;
   writeln( f );

   Mulk_Yes( 28, 5, 40, 13, Green, Black );
   Window( 29, 11, 51, 16 );
   writeln;
   Writeln( '     Global Score' );          writeln;
   Write( '   Processed  00  %' );
   Mulk_Yes( 28, 4, 62, 5, Red, Black );
   Window( 50, 3, 75, 7 );
   writeln;
   Writeln( '  Runhe-Kutt integration' );   writeln;
   Write( '     Processed  00  %' );        Kur( Off );
   for i:=1 to N_Work do
      begin
         writeln( f, '  Ephemerid of Glonass Satellite N ', Work_Sat[i].N,
                                            ' for date: ', Dat_Pro_Str );
         writeln( f );
         writeln( f, '  Time    Vx, [m/s]  Vy, [m/s]  Vz, [m/s]   '+
                                '   x, [m]       y, [m]       z, [m]' );
         Trass_NKA_Alm( Glonass_Sat[Work_Sat[i].N], Work_Sat[i].Tb, Point );
         Bet:=Angle_Plase( Plase, Point );
         with Work_Sat[i] do Count:=Round( ( Te-Tb )/Step );
         Tcr:=0.0;
         Time_To_Str(Time1, Work_Sat[i].Tb);
         with Point do writeln( f, Time1, Vx:11:4, Vy:11:4, Vz:11:4,
                                             x:13:1,  y:13:1,  z:13:1);
         for j:=1 to Count do
            begin
               if (Work_Sat[i].Tb+J*Step-Tcr) > 86400.0 then Tcr:=Tcr+86400.0;
               Time_to_Str( TimeSt, Work_Sat[i].Tb+j*Step-Tcr );
               RKGS( Point, Point_Gr, Step );
               Point:=Point_Gr;
               if Bet > Angle_Vis then c:='*' else c:=' ';
               Win( 55, 3, 75, 7, Red, Black );
               GotoXY( 11, 4 );    Write( Round( 100.0*j/Count ):3 );
               Kur( Off );
               with Point do writeln( f, TimeSt, Vx:11:4, Vy:11:4, Vz:11:4,
                                             x:13:1,  y:13:1,  z:13:1 );
            end;
         Win( 30, 12, 50, 15, Green, Black );
         GotoXY( 13, 3 );          Write( Round( 100.0*i/N_Work ):3 );
         Kur( Off );
         writeln( f );             writeln( f );
      end;
   close( f );
   Mulk_No( 28, 4, 62, 5, Red, Black );
   Mulk_No( 28, 5, 40, 13, Green, Black );
   win( 1, 2, 80, 25, Bc, Tc );                              clrscr;
   Mulk_Yes( 44, 5, 40, 12, Cyan, Black );                   writeln;
   writeln('    NES - ephemerides must be recount now.');    writeln;
   writeln('  Press any key for run or ESC for other ...');
   Kur( Off );
   c:=ReadKey;
   Mulk_No( 44, 5, 40, 12, Cyan, Black );
   win( 1, 2, 80, 25, Bc, Tc );                             clrscr;
   if c = #27 then Halt( 99 ) else Halt( 100+NChoise );

Ending:
   win( 1, 1, 80, 25, Black, White );     SuccessSound;
   clrscr;                                Halt( 99 )
end.
