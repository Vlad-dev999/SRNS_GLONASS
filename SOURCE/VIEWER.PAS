{$V-}

{$L cga.obj}
{$L egavga.obj}

program VIEWER;       {    View output information  }
uses Crt, Dos, Graph;

type Float = real;

const Npoint = 800;
          Ti = 8;
      SL_Mes = '     Vieuwer for graphical materials. V-2.0.           ' +
               '     Kharkov, 1995.';
         Sof = SizeOf( float );


type
     OnOff = ( On, Off );
     Efm_P = record
           Vx, Vy, Vz: float;     { Проекции вектора скорости, [м/с] }
              x, y, z: float;     { Координаты спутника, [м] }
         end;
     Efm_P_Ptr = ^Efm_P;
      Ptr_Delt = array[1..Npoint] of ^Efm_P;
        Sat_ar = array[1..Npoint] of Efm_P_Ptr;
    Sat_ar_Ptr = ^Sat_ar;
     max_array = array[1..65520] of byte;
    max_ar_ptr = ^max_array;
    type ar_fl = array[1..Sof] of byte;
     ar_fl_ptr = ^ar_Fl;

var
  sn, Time_Int, Date2, Time1, Time2, DateSt: String[10];
   Dir: DirStr;      Name: NameStr;     Ext: ExtStr;
   Pa: PathStr;        st: string;       Na: string[35];
            cd, k, i, j, Gd, Gm, maxX, maxY: integer;
                 N_Work, hh, mm, ss, Bc, Tc: byte;
                      tk1, tk2, Step1, Step: float;
                         Path, InpF1, InpF2: String[79];
                           Disp_Lk, delt_Lk: Ptr_Delt;
                              Message, Sstr: String;
                             CMode, Success: boolean;
                                     f1, f2: text;
                                          N: word;
                                          c: char;


function Rebyte( s: max_ar_ptr; n: byte ): float;
var dp: ar_fl;
     d: ^float;
     i: byte;
begin
   for i:=1 to Sof do dp[i]:=s^[Sof*(n-1)+i];
   d:=@dp;
   Rebyte:=d^;
end;

procedure InByte( s: max_ar_ptr; n: byte; Val: ar_fl_ptr );
var i: byte;
begin
   for i:=1 to Sof do s^[Sof*(n-1)+i]:=Val^[i]
end;

procedure Win ( x1, y1, x2, y2, Bc, Tc : byte ) ;
begin
   Window( x1, y1, x2, y2 );
   TextBackground( Bc );            TextColor( Tc )
end;

procedure cga_drv; external;

procedure egavga_drv; external;

procedure Graph_all( Delt: Ptr_Delt; ch: char );
type      aa = array[1..Npoint] of Float;
      Ptr_ar = ^aa;

var  min, max: Efm_P;
         mass: Ptr_ar;
         Mess: String[80];
    x1, y1, i: word;
       Signum: boolean;

procedure Norm( var Delt: Ptr_Delt );
var Valmax, Valmin, Val, s: float;
                      i, j: word;
procedure Roundation( var Num: float; c: char );
var i, pow, ab: integer;
begin
   if Num < 0.0 then ab:=-1 else ab:=1;
   if Num <> 0.0 then
      begin
         if Abs( Num ) < 1.0 then
            begin
               pow:=0;
               while Abs( Num ) < 1.0 do
                  begin
                     Num:=Num*10;   Inc( pow )
                  end;
               if ( Num > 0.0 ) and ( c = 'i' ) then Num:=Int( Num )
                             else Num:=Int( Num )+1.0*ab;
               for i:=1 to pow do Num:=Num/10
            end
              else if Abs( Num ) > 1.0 then
            begin
               pow:=0;
               while Abs( Num ) > 1.0 do
                  begin
                     Num:=Num/10;   Inc( pow )
                  end;
               Num:=Num*10;         Dec( pow );
               if ( Num > 0.0 ) and ( c = 'i' ) then Num:=Int( Num )-1.0
                             else Num:=Int( Num )+1.0*ab;
               for i:=1 to pow do Num:=Num*10
            end
      end
end;

begin
   max:=Delt[1]^;                   min:=max;
   for j:=1 to 6 do
      begin
         Valmax:=Rebyte( @max, j );     Valmin:=Rebyte( @min, j );
         for i:=1 to N do
            begin
               Val:=Rebyte( @Delt[i]^, j );
               if Val > Valmax then Valmax:=Val;
               if Val < Valmin then Valmin:=Val
            end;
         Roundation( Valmax, 'a' );       Roundation( Valmin, 'i' );
         InByte( @max, j, @Valmax );      InByte( @min, j, @Valmin );
         s:=Valmax-Valmin;
         if s <> 0.0 then for i:=1 to N do
            begin
               Val:=( ReByte( @Delt[i]^, j )-ReByte( @Min, j ) )/s;
               InByte( @Delt[i]^, j, @Val )
            end
      end
end;

procedure Graph_one( var Mas: AA; ss: byte; Mes: String );
var xk, yk, pow, px, py, x1, y1, i, j, k: integer;
                      Ymax, Ymin, zx, zy: float;

procedure GetCenter( Xmin, Xmax: float; var k, x1: integer; var z1: float );
   var max, min: integer;
             z: real;
   begin
      max:=30;     min:=maxY-19;
      if ( Xmax>0.0 ) and ( Xmin<0.0 ) then
         begin
            z:=( Xmax-Xmin )/10.0;     k:=0;       z1:=Xmin;
            repeat
               z1:=z1+z;               k:=k+1;
            until ( z1>=0.0 ) or ( k>=10 );
            x1:=maxY-19-Round( k*( MaxY-49 )/10 );
         end
                    else
                       if Xmax<=0.0 then x1:=max else x1:=min;
   end;

procedure OSI;
  var  pow, i, a, b, ay: integer;
     ym, ix, iy, yy, xx: float;
                 xc, yc: string[10];

begin
   if CMode then SetColor( Yellow );
   Rectangle( 0, 0, MaxX, MaxY+15 );
   Rectangle( 1, 1, MaxX-1, MaxY+14 );
   Line( 0, MaxY, maxX, maxY );
     x1:=85;
     line( 80, y1, 623, y1 );                      line( 600, y1+2, 623, y1 );
     line( x1, 5, x1, maxY-16 );                   line( 600, y1-2, 623, y1 );
     line( x1-3, 6+( MaxY div 20 ), x1, 6 );       line( x1+3, 6+( MaxY div 20 ), x1, 6 );
     SetLineStyle( Dottedln, 0, NormWidth );
     if CMode then SetColor( 15 );
     SetTextStyle( DefaultFont, Horizdir, 1 );
     OutTextXY( 595, y1-12, 't, s' );
     ix:=( N-1 )/5.0*Step;                         iy:=( Ymax-Ymin )/10.0;
     xx:=0.0;                                      yy:=Ymin-zy;
     pow:=0;                                       ym:=Ymax;
     if CMode then SetColor( LightGreen );
     while ( Abs( ym ) < 1.0 ) and ( ym <> 0.0 ) do
        begin
           ym:=ym*10;  yy:=yy*10;   iy:=iy*10;   Dec( pow )
        end;
     while ( Abs( ym+iy*10 ) > 10.0 ) and ( ym <> 0.0 ) do
        begin
           ym:=ym/10;  yy:=yy/10;   iy:=iy/10;   Inc( pow )
        end;
     Str( pow, xc );
     OutTextXY( x1, MaxY-11, '0' );
     OuttextXY( 20, 5, '1.0*E'+xc );
     for i:=1 to 5 do
        begin
           a:=i*100+85;
           if CMode then SetColor( LightRed );
           line( a, 30, a, maxY-19 );
           xx:=xx+ix;
           str( Round( xx ):3, xc );
           if CMode then SetColor( LightGreen );
           OuttextXY( a-20, maxY-11, xc );
        end;
     if CMode then SetColor( LightRed );
     for i:=0 to 10 do
        begin
           ay:=maxY-19-Round( i*( maxY-49 )/10 );   if i=py then
                                          begin
                                             if CMode then SetColor( LightGreen );
                                             OuttextXY( 35, ay-3, '0.0' );
                                             yy:=yy+iy;
                                             if CMode then SetColor( LightRed )
                                          end
                           else
              begin
                 line( 85, ay, 585, ay );
                 if CMode then SetColor( LightGreen );
                 str( yy:4:2, yc );
                 OuttextXY( 15, ay-3, yc );
                 if CMode then SetColor( LightRed );
                 yy:=yy+iy;
              end;
        end;
     if CMode then SetColor( White );
     SetLineStyle( SolidLn, 0, NormWidth );
  end;

begin
   px:=0;        py:=0;        x1:=0;        y1:=0;     zx:=0.0;    zy:=0.0;
   Ymax:= ReByte( @max, ss );      Ymin:=Rebyte( @min, ss );
   GetCenter( Ymin, Ymax, py, y1, zy );
   OSI;
   zy:=zy*( maxY-49 )/( Ymax-Ymin );
   yk:=MaxY-19-Round( Mas[1]*( maxY-49 )+zy );
   MoveTO( 85+Round( 1.0/N*( MaxX-189 )+zx ), yk );
   for i:=2 to N do
      begin
         xk:=85+Round( i/N*( MaxX-139 )+zx );
         yk:=MaxY-19-Round( Mas[i]*( MaxY-49 )+zy );
         lineto( xk, yk );
      end;
   if CMode then SetColor( LightMagenta );
   SetTextJustify( CenterText, CenterText );
   outtextxy( 320, 10, Mes );
   OutTextXY( maxX div 2, maxY+7, Message );
   SetTextJustify( HorizDir, TopText );
   if CMode then SetColor( White );
   Readln;
end;

begin
   Norm( Delt );
   New( Mass );
   for j:=1 to 6 do
      begin
         for i:=1 to N do Mass^[i]:=ReByte( @Delt[i]^, j );
         Case j of
               1: Mess:='Vx velocity';
               2: Mess:='Vy velocity';
               3: Mess:='Vz velocity';
               4: Mess:='x coordinate';
               5: Mess:='y coordinate';
               6: Mess:='z coordinate'
            end;
         Case j of
               1, 2, 3: Signum:=True;
               4, 5, 6: Signum:=False
            end;
         Case Upcase( ch ) of
               'E': Mess:=' Estimates of ' + Mess;
               'R': Mess:=' RMS errors of ' + Mess + ' estimation'
            end;
         if Signum then Mess:=Mess + ', m/s' else Mess:=Mess + ', m';
         ClearViewPort;
         Graph_one( Mass^, j, Mess )
      end;
   Dispose( Mass )
end;

procedure AvrSound;
const Song: array[1..5] of word = ( 70, 50, 100, 50, 70 );
var   j, i: integer;
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

procedure AvrExit ;
   begin
      AvrSound;TextBackground( 0 );ClrScr;Halt( 1 )
   end;

procedure ClrInfo;
var x, y : byte ;
begin
   x:=WhereX;          y:=WhereY;
   GotoXY( 1, 25 );    ClrEol;         GotoXY( x, y )
end;

procedure Read_Str( var s: string );
var
    Exit: boolean;
    x, y: integer;
      s1: string;
begin
   x:=WhereX;         y:=WhereY;
   TextColor( LightRed + Blink );
   write( s );        GotoXY( x, y );
   Exit:=False;
   TextColor( White );
   repeat
      if keypressed then
         begin
            clreol;        readln( s1 );       Exit:=True
         end
   until Exit;
   if s1='' then
      begin
         GotoXY( x, y );     writeln( s )
      end
            else s:=s1;
   TextColor( Tc )
end;

procedure Del_Space( var S: string );
begin
   while ( S[1]=' ' ) or ( S[1]='*' ) or ( S[1]=':' ) do Delete( S, 1, 1 )
end;

procedure Info( s: string ) ;
var x, y : byte ;
   begin
      x:=WhereX;                       y:=WhereY;
      Win( 1, 25, 80, 25, 7, Red );    ClrScr;
      Write( ' '+s );                  Win( 1, 2, 80, 25, 1, 7 );
      GotoXY( x, y );                  TextColor( Tc )
   end;

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

procedure Status_Line( s: string );
var x : byte ;
   begin
      Win( 1, 1, 80, 1, 7, 11 );    ClrScr;
      Write( s );                      x:=WhereX;
      Win( 1, 2, 80, 25, 1, 7 );       GotoXY( x, 2 );
      Kur( Off )
   end;


{*****************************     MAIN    ******************************}

begin
   Status_Line( SL_Mes );
   Bc:=Blue;     Tc:=Yellow;      Win( 1, 2, 80, 25, Bc, Tc );    clrscr;
   Pa:=FSearch( 'viewer.exe', GetEnv( 'PATH' ) );
   FSplit( FExpand( Pa ), Dir, Name, Ext );
   path:=Dir+'DATA\';
   Info( '  SPACE - modify;  ENTER - set screen mode. ' );
   CMode:=True;
   repeat
      GotoXY( 1, 1 );    clreol;
      write(' Screen mode: ');
      TextColor( White );
      if CMode then writeln('Color') else Writeln('Black & White');
      Kur( Off );
      c:=Readkey;
      TextColor( Tc );
      if c = ' ' then CMode:=Not CMode;
   until c=#13;
   clrscr;
   write( '  Path to input files : ' );    Read_Str( Path );
   InpF1:='estimate.nes';                  InpF2:='error.nes';
   Success:=False;
   repeat
      write( '   Name Input file ( for estimate ) : ' );     Read_Str( InpF1 );
      assign( f1, Path+InpF1 );
{$I-}
      Reset( f1 );
{$I+}
      if IOResult <> 0 then
         begin
            Success:=False;
            Info( 'Data file ( '+InpF1+' ) not found . . . Press any key' );
            AvrSound;          c:=Readkey;     clrscr;
         end
               else Success:=True;
   until Success;
   Success:=False;
   repeat
      write( '   Name Input file (  for  error  ) : ' );     Read_Str( InpF2 );
      assign( f2, Path+InpF2 );
{$I-}
      Reset( f2 );
{$I+}
      if IOResult <> 0 then
         begin
            Success:=False;
            Info( 'Data file ( '+InpF1+' ) not found . . . Press any key' );
            AvrSound;            c:=Readkey;     clrscr;
         end
               else Success:=True;
   until Success;
   Info( '     Reading  Data  from  '+InpF1+'  and  '+InpF2+'  files . . . ' );
   Kur( Off );
   Sstr:='';
   while Sstr='' do readln( f1, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   DateSt:=Copy( Sstr, 1, 8 );
   readln( f1, Sstr );
   while Sstr='' do readln( f1, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   Time1:=Copy( Sstr, 1, 8 );  Delete( Sstr, 1, 8 );    Del_Space( Sstr );
   while Sstr[1] = '-' do Delete( Sstr, 1, 1 );       Del_Space( Sstr );
   Message:='Date - ' +DateSt + '. Time int./' + Time1 + '-' + Sstr;
   Time2:=Copy( Sstr, 1, 8 );  Delete( Sstr, 1, 8 );
   Delete( Sstr, 1, 1 );       Del_Space( Sstr );
   while Sstr[1] <> ' ' do Delete( Sstr, 1 , 1 );
   i:=1;                     Del_Space( Sstr );
   while Sstr[i] <> ' ' do Inc( i );      Dec( i );
   Val( Copy( Sstr, 1, i ), Step, cd );
   Sstr:='';      while Sstr='' do Readln( f1, Sstr );
   Del_Space( Sstr );
   Message:=Message + ' ' + Sstr[1] + ' sat. ';
   Val( Copy( Sstr, 1, 1 ), N_Work, cd );    Sstr:='';
   while Sstr='' do readln( f2, Sstr );
   while Sstr[1] <> ':' do Delete( Sstr, 1, 1 );
   Del_Space( Sstr );
   Date2:=Copy( Sstr, 1, 8 );
   if Date2 <> DateSt then
      begin
         Info( '   Invalid  Date. Program halted. Press any key . . .' );
         AvrExit
      end;
   readln( f2, Sstr );
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
   i:=1;                     Del_Space( Sstr );
   while Sstr[i] <> ' ' do Inc( i );      Dec( i );
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
   Sstr:='';      while Sstr='' do Readln( f2, Sstr );
   Del_Space( Sstr );
   Val( Copy( Sstr, 1, 1 ), N, cd );    Sstr:='';
   if N_Work <> N then
      begin
         Info( '   Invalid Number of work satellites. ' +
                 'Program halted. Press any key . . .' );
         AvrExit
      end;
   N:=Round( ( tk2-tk1 )/Step )+1;
   if MemAvail < SizeOf( Efm_P )*N*2 then begin
             Info( '  No Free Memory. Program halted. Press any key . . . ' );
             AvrExit
          end
             else for i:=1 to N do
                begin
                   New( Delt_Lk[i] );    New( Disp_Lk[i] )
                end;
   Sstr:='';
   while Sstr='' do readln( f1, Sstr );        Sstr:='';
   while Sstr='' do readln( f2, Sstr );
   Str( N, sn );      Message:=Message + sn + ' samp.';
   for i:=1 to N do
      begin
         readln( f1, Sstr );
         with Delt_Lk[i]^ do
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
               while ( Sstr[k] <> ' ' ) and ( Ord( Sstr[0] ) > k ) do Inc( k );Dec( k );
               Val( Copy( Sstr, 1, k ), z, cd )
            end;
         readln( f2, Sstr );
         with Disp_Lk[i]^ do
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
               while ( Sstr[k] <> ' ' ) and ( Ord( Sstr[0] ) > k ) do Inc( k ); Dec( k );
               Val( Copy( Sstr, 1, k ), z, cd )
            end
      end;
   DetectGraph( Gd, Gm );
   if ( ( Gd = EGA ) or ( Gd = VGA ) ) then i:=RegisterBGIDriver( @egavga_drv )
                                       else i:=RegisterBGIDriver( @cga_drv );
   Gd:=Detect;
   initgraph( Gd, Gm, '' );
   maxX:=GetMaxX;     MaxY:=GetMaxY-15;
   SetColor( WHite );
   Graph_all( delt_Lk , 'e' );
   Graph_all( Disp_Lk , 'r' );
   CloseGraph;                           AvrSound;
   Win( 1, 1, 80, 25, Black, White );     clrscr
end.
