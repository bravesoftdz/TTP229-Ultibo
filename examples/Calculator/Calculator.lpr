program Calculator;

(*********************************** LICENCE **********************************\
| Copyright (c) 2014, A.E. TEC                                                 |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| * Redistributions of source code must retain the above copyright notice,     |
|   this list of conditions and the following disclaimer.                      |
| * Redistributions in binary form must reproduce the above copyright notice,  |
|   this list of conditions and the following disclaimer in the documentation  |
|   and/or other materials provided with the distribution.                     |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE    |
| LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          |
| CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         |
| SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     |
| INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      |
| CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      |
| ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   |
| POSSIBILITY OF SUCH DAMAGE.                                                  |
\******************************************************************************)
(************************************ USES ************************************\
| This sketch demonstrates the use of TTP229 Module, in 16 Buttons Mode.       |
| It displays the pressed number on the Terminal                               |
|                                                                              |
| It works with a TTP229 (16-Channel Digital Touch Capacitive Switch Sensor)   |
| using the 2-wires serial interface protocol - only 2 arduino pins.           |
\******************************************************************************)
(*********************************** CIRCUIT **********************************\
| 16 Buttons Multi Keys Mode:                                                  |
|  * TTP229 VCC to pin VCC                                                     |
|  * TTP229 GND to pin GND                                                     |
|  * TTP229 SCL to pin 2                                                       |
|  * TTP229 SDO to pin 3                                                       |
|  * TTP229 TP2 to GND via 1 Megohm resistor!                                  |
|  * TTP229 TP3 to GND via 1 Megohm resistor!                                  |
|  * TTP229 TP4 to GND via 1 Megohm resistor!                                  |
|  # See TTP229_Modes.jpg for help...                                          |
|                                                                              |
| Important:                                                                   |
|  * Must reconnect the TTP229 power so the mode changes will take effect      |
|  * The 1 Megohm resistors already exist on some TTP229 modules               |
\******************************************************************************)

uses
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  BCM2835,
  SysUtils,
  Classes,     {Include the common classes}
  GPIO,        {Include the GPIO unit to allow access to the functions}
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC,         {Include the MMC/SD core to access our SD card}
  BCM2708,     {And also include the MMC/SD driver for the Raspberry Pi}
  TTP229,
  LCD5110_Basic,
  DefaultFonts;

const 
  SCL_PIN = GPIO_PIN_23; // The pin number of the clock pin.
  SDO_PIN = GPIO_PIN_24; // The pin number of the data pin.

var
  WindowHandle:TWindowHandle; 
  myGLCD: TLCD5110;
  KeyPad: TTTP229;

procedure LogError(Msg: String);
var
  FileStream: TFileStream;
  StringList: TStringList;

const
  FileName = 'C:\LogErro.txt';
begin
  Msg := Trim(Msg);

  if Msg = '' then
    Exit;

  {We should check if the file exists first before trying to create it}
  ConsoleWindowWriteLn(WindowHandle,'Checking to see if ' + Filename + ' exists');

  if FileExists(Filename) then
   begin
    {If it does exist we can delete it}
    ConsoleWindowWriteLn(WindowHandle,'Deleting the file ' + Filename);
    DeleteFile(Filename);
   end;

  FileStream:=TFileStream.Create(Filename,fmCreate);

  try
    StringList:=TStringList.Create;

    try
      StringList.Add(Msg);
      StringList.SaveToStream(FileStream);
    finally
      StringList.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

procedure Setup();
begin
  {Let's create a console window so we can report what is happening}
  WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

  {Display a welcome message}
  ConsoleWindowWriteLn(WindowHandle,'Welcome to TTP229 Keypad Example');

  while not DirectoryExists('C:\') do
   begin
    {Sleep for a second}
    Sleep(1000);
   end;

  ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
  ConsoleWindowWriteLn(WindowHandle,'');

    try
      KeyPad := TTTP229.Create(SCL_PIN, SDO_PIN); // TTP229(sclPin, sdoPin)
    except
      on E: Exception do
      begin
        KeyPad := nil;
        ConsoleWindowWriteLn(WindowHandle, 'Setup() error: ' + E.Message);
        LogError(E.Message);
        Exit;
      end;
    end;
  
    try
      myGLCD := TLCD5110.Create(stBCM2835, $3F);
      myGLCD.SetFont(SmallFont);
    except
      on E: Exception do
      begin
        KeyPad.Free;
        myGLCD := nil;
        ConsoleWindowWriteLn(WindowHandle, 'Setup() error: ' + E.Message);
        LogError(E.Message);
        Exit;
      end;
    end;
  
  myGLCD.ClrScr();
  myGLCD.Print('Start Touching Several Keys Simultaneously!', LEFT, 0);
end;

procedure Loop();
var
  i, sum: Byte;
  keys: Word;
begin
  try
    sum := 0;
    keys := KeyPad.ReadKeys16(); // Blocking

    for i := 0 to 15 do
      if (keys and (1 shl i) = 1) then
        Inc(sum, i + 1);

    myGLCD.PrintNumI(sum, RIGHT, 16, 5, ' ');
  except
    on E: Exception do
    begin
      KeyPad.Free;
      myGLCD.Free;
      myGLCD := nil;
      ConsoleWindowWriteLn(WindowHandle, 'Loop() error: ' + E.Message);
      LogError(E.Message);
      Exit;
    end;
  end;
end;

begin
  Setup();

  while Assigned(KeyPad) do
    Loop();

  ConsoleWindowWriteLn(WindowHandle, 'Bye');

  {Halt the main thread if we ever get to here}
  ThreadHalt(0);
end.
 
