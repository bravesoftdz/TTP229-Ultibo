unit TTP229;

(************************************ USES ***************************************
| This class is designed to communicate with the TTP229 chip from a Raspberry Pi |
|                                                                                |
| It works with a TTP229 (16-Channel Digital Touch Capacitive Switch Sensor)     |
| using the 2-wires serial interface protocol                                    |
 ********************************************************************************)
(*********************************** CIRCUIT *************************************
| General:                                                                       |
|  * TTP229 VCC to pin VCC                                                       |
|  * TTP229 GND to pin GND                                                       |
|  * TTP229 SCL to any pin you choose...                                         |
|  * TTP229 SDO to any pin you choose...                                         |
|                                                                                |
| 16 Buttons Mode (else 8 Buttons Mode):                                         |
|  * TTP229 TP2 to GND via 1 Megohm resistornot                                  |
|  # Must use the ReadKey16, GetKeys16... else use the ReadKey8, GetKeys8...     |
|                                                                                |
| Multi Keys Mode (else Single key Mode):                                        |
|  * TTP229 TP3 to GND via 1 Megohm resistornot                                  |
|  * TTP229 TP4 to GND via 1 Megohm resistornot                                  |
|                                                                                |
| 64Hz Sampling Rate (else 8Hz Sampling Rate):                                   |
|  * TTP229 TP5 to GND via 1 Megohm resistornot                                  |
|                                                                                |
| 2ms Wake Up Rate (else 4ms Wake Up Rate):                                      |
|  * TTP229 TP6 to GND via 1 Megohm resistornot                                  |
|                                                                                |
| 60sec Key On Timeout (else No Key On Timeout):                                 |
|  * TTP229 TP7 to GND via 1 Megohm resistornot                                  |
|                                                                                |
| Important:                                                                     |
|  * Must reconnect the TTP229 power so the mode changes will take effect        |
|  * The 1 Megohm resistors already exist on some TTP229 modules                 |
 ********************************************************************************)

interface

type

  { TTTP229 }

  TTTP229 = class
  private
    _sclPin, _sdoPin: Byte;
    _key8, _keys8, _key16: Byte;
    _keys16: Word;
    procedure WaitForTouch();
    procedure Key8();
    procedure Keys8();
    procedure Key16();
    procedure Keys16();
    function GetBit(): Boolean;
    function IsTouch(): Boolean;
    procedure SleepMicroseconds(t: LongWord);
  public
    constructor Create(sclPin, sdoPin: Byte);
    function ReadKey8(): Byte;
    function GetKey8(): Byte;
    function ReadKeys8(): Byte;
    function GetKeys8(): Byte;
    function ReadKey16(): Byte;
    function GetKey16(): Byte;
    function ReadKeys16(): Word;
    function GetKeys16(): Word;
  end;

implementation

uses
  SysUtils, Platform, GlobalConst, GPIO, Devices, SPI;

constructor TTTP229.Create(sclPin, sdoPin: Byte);
begin
  _sclPin := sclPin;
  _sdoPin := sdoPin;

  GPIOFunctionSelect(_sclPin , GPIO_FUNCTION_OUT);
  GPIOFunctionSelect(_sdoPin , GPIO_FUNCTION_IN);

  GPIOOutputSet(_sclPin, GPIO_LEVEL_HIGH);
end;

function TTTP229.ReadKey8(): Byte;
begin
  WaitForTouch();
  Key8();
  Result := _key8;
end;

function TTTP229.GetKey8(): Byte;
begin
  if (IsTouch()) then
    Key8();

  Result := _key8;
end;

function TTTP229.ReadKeys8(): Byte;
begin
  WaitForTouch();
  Keys8();
  Result := _keys8;
end;

function TTTP229.GetKeys8(): Byte;
begin
  if (IsTouch()) then
   Keys8();

  Result := _keys8;
end;

function TTTP229.ReadKey16(): Byte;
begin
  WaitForTouch();
  Key16();
  Result := _key16;
end;

function TTTP229.GetKey16(): Byte;
begin
  if (IsTouch())  then
    Key16();

  Result := _key16;
end;

function TTTP229.ReadKeys16(): Word;
begin
  WaitForTouch();
  Keys16();
  Result := _keys16;
end;

function TTTP229.GetKeys16(): Word;
begin
  if (IsTouch())  then
    Keys16();

  Result := _keys16;
end;

procedure TTTP229.Key8();
var
  i: Byte;
begin
  _key8 := 0;

  for i := 0 to 7 do
    if (GetBit())  then
      _key8 := i + 1;

  Sleep(2); // Tout
end;

procedure TTTP229.Keys8();
var
  i: Byte;
begin
  _keys8 := 0;

  for i := 0 to 7 do
    if (GetBit())  then
      _keys8 := _keys8 or 1 shl i;

  Sleep(2); // Tout
end;

procedure TTTP229.Key16();
var
  i: Byte;
begin
  _key16 := 0;

  for i := 0 to 15 do
    if (GetBit())  then
      _key16 := i + 1;

  Sleep(2); // Tout
end;

procedure TTTP229.Keys16();
var
  i: Byte;
begin
  _keys16 := 0;

  for i := 0 to 15 do
    if (GetBit())  then
      _keys16 := _keys16 or 1 shl i;

  Sleep(2); // Tout
end;

function TTTP229.GetBit(): Boolean;
begin
  GPIOOutputSet(_sclPin, GPIO_LEVEL_LOW);
  SleepMicroseconds(2); // 500KHz
  Result := GPIOInputGet(_sdoPin) <> 0;
  GPIOOutputSet(_sclPin, GPIO_LEVEL_HIGH);
  SleepMicroseconds(2); // 500KHz
end;

function TTTP229.IsTouch(): Boolean;
var
  timeout: Word;
begin
  timeout := 5000; // 50ms timeout

  while (GPIOInputGet(_sdoPin) <> 0) do // DV GPIO_LEVEL_LOW
  begin
    Dec(timeout);

    if (timeout = 0) then
    begin
      Result := false;
      Exit;
    end;

    SleepMicroseconds(10);
  end;

  while (GPIOInputGet(_sdoPin) = 0) do // DV GPIO_LEVEL_HIGH
  begin
    Dec(timeout);

    if (timeout = 0) then
    begin
      Result := false;
      Exit;
   end;

    SleepMicroseconds(10);
  end;

  SleepMicroseconds(10); // Tw
  Result := true;
end;

procedure TTTP229.SleepMicroseconds(t: LongWord);
var
  e: QWord;
begin
  e := GetTickCount64;

  while GetTickCount64 - e < t do;
end;

procedure TTTP229.WaitForTouch();
begin
  while (GPIOInputGet(_sdoPin) <> 0) do ; // DV GPIO_LEVEL_LOW

  while (GPIOInputGet(_sdoPin) = 0) do ; // DV GPIO_LEVEL_HIGH

  SleepMicroseconds(10); // Tw
end;

end.
