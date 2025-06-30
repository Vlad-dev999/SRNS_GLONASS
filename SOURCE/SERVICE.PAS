unit service;
{$L kursor.obj}
{$L cga.obj}
{$L egavga.obj}
interface
procedure kuroff;
procedure kuron;
procedure cga_drv;
procedure egavga_drv;

implementation
procedure kuroff; external;
procedure kuron; external;
procedure cga_drv; external;
procedure egavga_drv; external;
end.

