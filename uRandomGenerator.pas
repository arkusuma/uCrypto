unit uRandomGenerator;

interface

uses
  Windows, uHash;

type
  TRandomGenerator = class
  private
    MD5: TMD5;
    procedure FeedGenerator;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RandomBytes(var Buffer; Length: Integer);
  end;

implementation

constructor TRandomGenerator.Create;
begin
  MD5 := TMD5.Create;
end;

destructor TRandomGenerator.Destroy;
begin
  MD5.Free;
end;

procedure TRandomGenerator.RandomBytes(var Buffer; Length: Integer);
type
  TBytes = array[0..MaxInt - 1] of Byte;
var
  I: Integer;
  X: Integer;
  D: TDigest;
begin
  for I := 0 to Length - 1 do
  begin
    FeedGenerator;
    D := MD5.Digest;
    X := D.Bytes[0] xor MD5.Digest[1] xor MD5.Digest[2] xor MD5.Digest[3];
    TBytes(Buffer)[I] := X and 255;
  end;
end;

procedure TRandomGenerator.FeedGenerator;
begin
  Inc(PCardinal(MD5.Digest.Buffer)^, GetTickCount);
  MD5.Update(MD5.Digest.Buffer, MD5.Digest.Length); 
end;

initialization
  Randomize;

end.
