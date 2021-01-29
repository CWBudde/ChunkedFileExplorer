unit CfeChunkMp4;

interface

uses
  Classes, SysUtils, CfeChunkCommon;

type
  TMp4DefinedChunk = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TMp4UnknownChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

  TMp4FileTypeChunk = class(TMp4DefinedChunk)
  private
    FMajorBrand: TChunkName;
    FMinorVersion: Integer;
    FCompatibleBrands: array of TChunkName;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4FreeChunk = class(TMp4DefinedChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TFileMp4 = class(TChunkedFile)
  private
    FFileType: TMp4FileTypeChunk;
    procedure ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName;
      ChunkSize: Cardinal);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;
    class function CanLoad(Stream: TStream): Boolean;
  end;

implementation

{ TMp4DefinedChunk }

constructor TMp4DefinedChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4UnknownChunk }

constructor TMp4UnknownChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4FreeChunk }

class function TMp4FreeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'free';
end;


{ TMp4FileTypeChunk }

class function TMp4FileTypeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'ftyp';
end;

procedure TMp4FileTypeChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  CompatibleBrandCount: Integer;
begin
  inherited;

  Stream.Read(FMajorBrand, 4);
  Stream.Read(FMinorVersion, 4);
  CompatibleBrandCount := (FChunkSize - 16) div SizeOf(Integer);
  SetLength(FCompatibleBrands, CompatibleBrandCount);
  for Index := 0 to CompatibleBrandCount - 1 do
    Stream.Read(FCompatibleBrands[Index], 4);
end;

procedure TMp4FileTypeChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FMajorBrand, 4);
  Stream.Write(FMinorVersion, 4);
  for Index := 0 to Length(FCompatibleBrands) - 1 do
    Stream.Write(FCompatibleBrands[Index], 4);
end;


{ TFileMp4 }

constructor TFileMp4.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder];
  FFileType := TMp4FileTypeChunk.Create;
end;

destructor TFileMp4.Destroy;
begin

  inherited;
end;

class function TFileMp4.CanLoad(Stream: TStream): Boolean;
var
  ChunkSize: Cardinal;
  ChunkName: TChunkName;
begin
  Result := Stream.Size >= 8;

  if Result then
  begin
    Stream.Read(ChunkSize, 4);
    Stream.Read(ChunkName, 4);
    Stream.Seek(-8, soFromCurrent);
    Result := ChunkName = GetClassChunkName;
  end;
end;

class function TFileMp4.GetClassChunkName: TChunkName;
begin
  Result := 'ftyp';
end;

procedure TFileMp4.ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName;
  ChunkSize: Cardinal);
var
  ChunkClass: TDefinedChunkClass;
  DefinedChunk: TDefinedChunk;
  UnknownChunk: TMp4UnknownChunk;
begin
  ChunkClass := ChunkClassByChunkName(ChunkName);
  if Assigned(ChunkClass) then
  begin
    DefinedChunk := ChunkClass.Create;
    DefinedChunk.LoadFromStream(Stream);
    FChunkList.Add(DefinedChunk);
  end
  else
  begin
    UnknownChunk := TMp4UnknownChunk.Create;
    UnknownChunk.LoadFromStream(Stream);
    AddChunk(UnknownChunk);
  end;
end;

procedure TFileMp4.LoadFromStream(Stream: TStream);
var
  ChunkSize: Cardinal;
  ChunkName: TChunkName;
  UnknownChunk: TMp4UnknownChunk;
begin
  FFileType.LoadFromStream(Stream);
  AddChunk(FFileType);

  while Stream.Position + 8 <= Stream.Size do
  begin
    Stream.Read(ChunkSize, 4);
    Stream.Read(ChunkName, 4);
    Flip32(ChunkSize);
    Stream.Position := Stream.Position - 8;
    ReadUnknownChunk(Stream, ChunkName, ChunkSize);
  end;
end;

procedure TFileMp4.SaveToStream(Stream: TStream);
begin
  FFileType.SaveToStream(Stream);
end;

initialization

  TFileMp4.RegisterChunks([TMp4FreeChunk]);

end.
