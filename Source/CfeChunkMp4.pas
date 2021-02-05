unit CfeChunkMp4;

interface

uses
  Classes, SysUtils, Contnrs, CfeChunkCommon;

type
  TMp4DefinedChunk = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TMp4FixedDefinedChunk = class(TFixedDefinedChunk)
  public
    constructor Create; override;
  end;

  TMp4UnknownChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

  TMp4ContainerChunk = class(TChunkContainer)
  public
    constructor Create; override;
  end;

  TMp4TextChunk = class(TCustomTextChunk)
  public
    constructor Create; override;
  end;

  TMp4FileTypeChunk = class(TMp4DefinedChunk)
  private
    FMajorBrand: TChunkName;
    FMinorVersion: Integer;
    FCompatibleBrands: array of TChunkName;
    function GetCompatibleBrandsAsString: String;
  protected
    function GetChunkSize: Cardinal; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;

    property MajorBrand: TChunkName read FMajorBrand;
    property MinorVersion: Integer read FMinorVersion;
    property CompatibleBrandsAsString: String read GetCompatibleBrandsAsString;
  end;

  TMp4FreeChunk = class(TMp4DefinedChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MovieDataChunk = class(TMp4DefinedChunk)
  private
    FStream: TStream;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4MovieHeaderChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FCreationTime: Cardinal;
    FModificationTime: Cardinal;
    FTimeScale: Cardinal;
    FDuration: Cardinal;
    FPreferredRate: Cardinal;
    FPreferredVolume: Word;
    FReserved: array [0..9] of Byte;
    FMatrixStructure: array [0..8] of Integer;
    FPreviewTime: Cardinal;
    FPreviewDuration: Cardinal;
    FPosterTime: Cardinal;
    FSelectionTime: Cardinal;
    FSelectionDuration: Cardinal;
    FCurrentTime: Cardinal;
    FNextTrackId: Cardinal;
    function GetCreationTime: TDateTime;
    function GetDurationAsFloat: Single;
    function GetModificationTime: TDateTime;
    function GetPreferredRateAsFloat: Single;
    function GetPreferredVolumeAsFloat: Single;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property TimeScale: Cardinal read FTimeScale;
    property Duration: Single read GetDurationAsFloat;
    property PreferredRate: Single read GetPreferredRateAsFloat;
    property PreferredVolume: Single read GetPreferredVolumeAsFloat;
  end;

  TMp4InitialObjectDescriptorChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FInitialObjectDescriptorTag: Byte;
    FExtendedDescriptorTypeTag: array [0..2] of Byte;
    FDescriptorTypeLength: Byte;
    FObjectDescriptorID: Word;
    FObjectDescriptorProfileLevel: Byte;
    FSceneProfileLevel: Byte;
    FAudioProfileLevel: Byte;
    FVideoProfileLevel: Byte;
    FGraphicsProfileLevel: Byte;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4TrackHeaderChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FCreationTime: Cardinal;
    FModificationTime: Cardinal;
    FTrackID: Cardinal;
    FReservedA: Cardinal;
    FDuration: Cardinal;
    FReservedB: array [0..7] of Byte;
    FLayer: Word;
    FAlternateGroup: Word;
    FVolume: Word;
    FReservedC: Word;
    FMatrixStructure: array [0..8] of Integer;
    FTrackWidth: Cardinal;
    FTrackHeight: Cardinal;
    function GetCreationTime: TDateTime;
    function GetModificationTime: TDateTime;
    function GetVolumeAsFloat: Single;
    function GetIsEnabled: Boolean;
    function GetIsInMovie: Boolean;
    function GetIsInPoster: Boolean;
    function GetIsInPreview: Boolean;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property IsEnabled: Boolean read GetIsEnabled;
    property IsInMovie: Boolean read GetIsInMovie;
    property IsInPreview: Boolean read GetIsInPreview;
    property IsInPoster: Boolean read GetIsInPoster;
    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property TrackWidth: Cardinal read FTrackWidth;
    property TrackHeight: Cardinal read FTrackHeight;
    property TrackID: Cardinal read FTrackID;
    property Duration: Cardinal read FDuration;
    property Layer: Word read FLayer;
    property AlternateGroup: Word read FAlternateGroup;
    property Volume: Single read GetVolumeAsFloat;
  end;

  TMp4EditListEntry = class
  private
    FTrackDuration: Cardinal;
    FMediaTime: Cardinal;
    FMediaRate: Cardinal;
    function GetMediaRateAsFloat: Single;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property TrackDuration: Cardinal read FTrackDuration;
    property MediaTime: Cardinal read FMediaTime;
    property MediaRate: Single read GetMediaRateAsFloat;
  end;

  TMp4EditListEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4EditListEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4EditListEntry);
  public
    function Add(AChunk: TMp4EditListEntry): Integer;
    function Extract(Item: TMp4EditListEntry): TMp4EditListEntry;
    function Remove(AChunk: TMp4EditListEntry): Integer;
    function IndexOf(AChunk: TMp4EditListEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4EditListEntry);
    function First: TMp4EditListEntry;
    function Last: TMp4EditListEntry;

    property Items[Index: Integer]: TMp4EditListEntry read GetItem write SetItem; default;
  end;

  TMp4EditListChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FList: TMp4EditListEntryList;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property EditList: TMp4EditListEntryList read FList;
  end;

  TMp4EditChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MediaHeaderChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FCreationTime: Cardinal;
    FModificationTime: Cardinal;
    FTimeScale: Cardinal;
    FDuration: Cardinal;
    FLanguage: Word;
    FQuality: Word;
    function GetCreationTime: TDateTime;
    function GetDuration: Single;
    function GetModificationTime: TDateTime;
    function GetTimeScale: Single;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CreationTime: TDateTime read GetCreationTime;
    property ModificationTime: TDateTime read GetModificationTime;
    property TimeScale: Single read GetTimeScale;
    property Duration: Single read GetDuration;
  end;

  TMp4HandlerReferenceChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FComponentType: TChunkName;
    FComponentSubType: TChunkName;
    FComponentManufacturer: TChunkName;
    FComponentFlags: array [0..3] of Byte;
    FComponentFlagMask: array [0..3] of Byte;
    FComponentName: AnsiString;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ComponentType: TChunkName read FComponentType;
    property ComponentSubType: TChunkName read FComponentSubType;
    property ComponentManufacturer: TChunkName read FComponentManufacturer;
  end;

  TMp4VideoMediaInformationChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGraphicsMode: Word;
    FOpColor: array [0..2] of Word;
    function GetOpColor(Index: Byte): Word;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraphicsMode: Word read FGraphicsMode;
    property OpColor[Index: Byte]: Word read GetOpColor;
  end;

  TMp4SoundMediaInformationChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FBalance: Word;
    FReserved: Word;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Balance: Word read FBalance;
  end;

  TMp4BaseMediaInformationChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGraphicsMode: Word;
    FOpColor: array [0..2] of Word;
    FBalance: Word;
    FReserved: Word;
    function GetOpColor(Index: Byte): Word;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GraphicsMode: Word read FGraphicsMode;
    property OpColor[Index: Byte]: Word read GetOpColor;
    property Balance: Word read FBalance;
  end;

  TMp4DataReferenceUrlChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4DataReferenceChunk = class(TMp4ContainerChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4StreamTypeID = (
    stObjectDescriptor = 1,
    stClockReference = 2,
    stSceneDescriptor = 3,
    stVisual = 4,
    stAudio = 5,
    stMpeg7 = 6,
    stIPMP = 7,
    stOCI = 8,
    stMpegJava = 9
  );

  TMp4ElementaryStreamDescriptorChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FElementaryStreamDescriptorTag: Byte;
    FExtendedDescriptorTypeTag: array [0..2] of Byte;
    FElementaryDescriptorID: Word;
    FDescriptorTypeLength: Byte;
    FStreamPriority: Byte;
    FDecoderConfigDescriptor: Byte;
    FExtendedDecoderConfigDescriptor: array [0..2] of Byte;
    FDecoderConfigDescriptorTypeLength: Byte;
    FObjectTypeId: Byte;
    FStreamType: TMp4StreamTypeID;
    FUpstreamFlag: Boolean;
    FBufferSize: Cardinal;
    FMaximumBitRate: Cardinal;
    FAverageBitRate: Cardinal;
    FDecoderSpecificTypeValue: Byte;
    FExtendedDecoderSpecificTypeValue: array [0..2] of Byte;
    FDecoderSpecificTypeLength: Byte;
    FDecoderSpecificData: TMemoryStream;
    FConfigDescriptorType: Byte;
    FExtendedConfigDescriptorType: array [0..2] of Byte;
    FConfigDescriptorLength: Byte;
    FConfigValue: Byte;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property StreamPriority: Byte read FStreamPriority;
    property StreamType: TMp4StreamTypeID read FStreamType;
    property UpstreamFlag: Boolean read FUpstreamFlag;
    property BufferSize: Cardinal read FBufferSize;
    property MaximumBitRate: Cardinal read FMaximumBitRate;
    property AverageBitRate: Cardinal read FAverageBitRate;
    property DecoderSpecificData: TMemoryStream read FDecoderSpecificData;
  end;

  TMp4DataInformationChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4Mp4aChunk = class(TMp4ContainerChunk)
  private
    FReserved: array [0..5] of Byte;
    FDataReferenceIndex: Cardinal;
    FEncodingVersionLevel: Word;
    FEncodingRevisionLevel: Word;
    FEncodingVendor: TChunkName;
    FChannelCount: Cardinal;
    FSampleSize: Cardinal;
    FAudioCompressionId: Word;
    FAudioPacketSize: Word;
    FSampleRate: Cardinal;
    function GetSampleRate: Single;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property DataReferenceIndex: Cardinal read FDataReferenceIndex;
    property ChannelCount: Cardinal read FChannelCount;
    property SampleSize: Cardinal read FSampleSize;
    property SampleRate: Single read GetSampleRate;
    property AudioCompressionId: Word read FAudioCompressionId;
    property AudioPacketSize: Word read FAudioPacketSize;
  end;

  TMp4SampleDescriptionChunk = class(TMp4ContainerChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4SampleTableEntry = class
  private
    FSampleCount: Cardinal;
    FSampleDuration: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property SampleCount: Cardinal read FSampleCount;
    property SampleDuration: Cardinal read FSampleDuration;
  end;

  TMp4SampleTableEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleTableEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleTableEntry);
  public
    function Add(AChunk: TMp4SampleTableEntry): Integer;
    function Extract(Item: TMp4SampleTableEntry): TMp4SampleTableEntry;
    function Remove(AChunk: TMp4SampleTableEntry): Integer;
    function IndexOf(AChunk: TMp4SampleTableEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleTableEntry);
    function First: TMp4SampleTableEntry;
    function Last: TMp4SampleTableEntry;

    property Items[Index: Integer]: TMp4SampleTableEntry read GetItem write SetItem; default;
  end;

  TMp4TimeToSampleChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FSampleTable: TMp4SampleTableEntryList;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property SampleTable: TMp4SampleTableEntryList read FSampleTable;
  end;

  TMp4SyncSampleChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FSyncSampleTable: array of Cardinal;
    function GetCount: Cardinal;
    function GetSyncSample(Index: Integer): Cardinal;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property SyncSample[Index: Integer]: Cardinal read GetSyncSample;
    property Count: Cardinal read GetCount;
  end;

  TMp4SampleToChunkEntry = class
  private
    FFirstChunk: Cardinal;
    FSamplesPerChunk: Cardinal;
    FSampleDescription: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property FirstChunk: Cardinal read FFirstChunk;
    property SamplesPerChunk: Cardinal read FSamplesPerChunk;
    property SampleDescription: Cardinal read FSampleDescription;
  end;

  TMp4SampleToChunkEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleToChunkEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleToChunkEntry);
  public
    function Add(AChunk: TMp4SampleToChunkEntry): Integer;
    function Extract(Item: TMp4SampleToChunkEntry): TMp4SampleToChunkEntry;
    function Remove(AChunk: TMp4SampleToChunkEntry): Integer;
    function IndexOf(AChunk: TMp4SampleToChunkEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleToChunkEntry);
    function First: TMp4SampleToChunkEntry;
    function Last: TMp4SampleToChunkEntry;

    property Items[Index: Integer]: TMp4SampleToChunkEntry read GetItem write SetItem; default;
  end;

  TMp4SampleToChunkChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FSampleToChunkTable: TMp4SampleToChunkEntryList;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property SampleToChunkTable: TMp4SampleToChunkEntryList read FSampleToChunkTable;
  end;

  TMp4SampleSizeChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FSampleSize: Cardinal;
    FNumberOfEntries: Cardinal;
    FSampleSizes: array of Cardinal;
    function GetCount: Cardinal;
    function GetSampleSize(Index: Integer): Cardinal;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property SampleSize[Index: Integer]: Cardinal read GetSampleSize;
    property Count: Cardinal read GetCount;
  end;

  TMp4ChunkOffsetChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FNumberOfEntries: Cardinal;
    FChunkOffsetTable: array of Int64;
    function GetChunkOffset(Index: Integer): Int64;
    function GetCount: Cardinal;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ChunkOffset[Index: Integer]: Int64 read GetChunkOffset;
    property Count: Cardinal read GetCount;
  end;

  TMp4SampleGroupDescriptionChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGroupingType: TChunkName;
    FDefaultLength: Cardinal;
    FEntryCount: Cardinal;
    FPayload: array of SmallInt;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property DefaultLength: Cardinal read FDefaultLength;
    property EntryCount: Cardinal read FEntryCount;
  end;

  TMp4SampleToGroupEntry = class
  private
    FSampleCount: Cardinal;
    FGroupDescriptionIndex: Cardinal;
  public
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);

    property SampleCount: Cardinal read FSampleCount write FSampleCount;
    property GroupDescriptionIndex: Cardinal read FGroupDescriptionIndex write FGroupDescriptionIndex;
  end;

  TMp4SampleToGroupEntryList = class(TObjectList)
  protected
    function GetItem(Index: Integer): TMp4SampleToGroupEntry;
    procedure SetItem(Index: Integer; AChunk: TMp4SampleToGroupEntry);
  public
    function Add(AChunk: TMp4SampleToGroupEntry): Integer;
    function Extract(Item: TMp4SampleToGroupEntry): TMp4SampleToGroupEntry;
    function Remove(AChunk: TMp4SampleToGroupEntry): Integer;
    function IndexOf(AChunk: TMp4SampleToGroupEntry): Integer;
    procedure Insert(Index: Integer; AChunk: TMp4SampleToGroupEntry);
    function First: TMp4SampleToGroupEntry;
    function Last: TMp4SampleToGroupEntry;

    property Items[Index: Integer]: TMp4SampleToGroupEntry read GetItem write SetItem; default;
  end;

  TMp4SampleToGroupChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGroupingType: TChunkName;
    FEntryCount: Cardinal;
    FTableData: TMp4SampleToGroupEntryList;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property GroupingType: TChunkName read FGroupingType;
    property TableData: TMp4SampleToGroupEntryList read FTableData;
  end;

  TMp4CompactSampleToGroupChunk = class(TMp4DefinedChunk)
  private
    FVersion: Byte;
    FFlags: array [0..2] of Byte;
    FGroupingType: TChunkName;
    FUnknown: Cardinal;
    FPatternLengthSize: Cardinal;
    FSampleCountSize: Cardinal;
    FNumberOfFramesFirstPeriod: Word;
    FNumberOfFrames: Cardinal;
    FEntryCount: Cardinal;
    FPayload: array of SmallInt;
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PatternLengthSize: Cardinal read FPatternLengthSize;
    property SampleCountSize: Cardinal read FSampleCountSize;
    property EntryCount: Cardinal read FEntryCount;
  end;

  TMp4SampleTableChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4MediaInformationChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MediaChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4TrackChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4DataChunk = class(TMp4DefinedChunk)
  private
    FType: Cardinal;
    FLocale: Cardinal;
    FData: AnsiString;
  protected
    function GetChunkSize: Cardinal; override;
  public
    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property DataAsString: AnsiString read FData;
    property &Type: Cardinal read FType;
    property Locale: Cardinal read FLocale;
  end;

  TMp4CustomItemListChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TMp4ItemListArtChunk = class(TMp4CustomItemListChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4ItemListTooChunk = class(TMp4CustomItemListChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4ItemListChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MetaChunk = class(TMp4ContainerChunk)
  private
    FUnknown: Cardinal;
  protected
    function GetChunkSize: Cardinal; override;
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TMp4UserDataChunk = class(TMp4ContainerChunk)
  public
    constructor Create; override;

    class function GetClassChunkName: TChunkName; override;
  end;

  TMp4MoovChunk = class(TMp4ContainerChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  public
    constructor Create; override;
  end;

  TFileMp4 = class(TChunkedFile)
  private
    FFileType: TMp4FileTypeChunk;
    procedure ReadUnknownChunk(Stream: TStream; ChunkName: TChunkName;
      ChunkSize: Cardinal);
  public
    constructor Create; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function GetClassChunkName: TChunkName; override;
    class function CanLoad(Stream: TStream): Boolean;
  end;

implementation

uses
  DateUtils, CfeUtils;

resourcestring
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';

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


{ TMp4FixedDefinedChunk }

constructor TMp4FixedDefinedChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4ContainerChunk }

constructor TMp4ContainerChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4TextChunk }

constructor TMp4TextChunk.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder, cfIncludeChunkInSize];
end;


{ TMp4FreeChunk }

class function TMp4FreeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'free';
end;


{ TMp4MovieDataChunk }

constructor TMp4MovieDataChunk.Create;
begin
  inherited;
  FStream := TMemoryStream.Create;
end;

destructor TMp4MovieDataChunk.Destroy;
begin
  FreeAndNil(FStream);
  inherited;
end;

procedure TMp4MovieDataChunk.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TMp4MovieDataChunk then
  begin
    FStream.Position := 0;
    TMp4MovieDataChunk(Dest).FStream.Position := 0;
    TMp4MovieDataChunk(Dest).FStream.CopyFrom(FStream, FStream.Size);
  end;
end;

function TMp4MovieDataChunk.GetChunkSize: Cardinal;
begin
  Result := FStream.Size;
end;

class function TMp4MovieDataChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mdat';
end;

procedure TMp4MovieDataChunk.LoadFromStream(Stream: TStream);
begin
  inherited;
  FStream.Position := 0;
  FStream.CopyFrom(Stream, FChunkSize);
  FStream.Position := 0;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4MovieDataChunk.SaveToStream(Stream: TStream);
begin
  inherited;
  FStream.Position := 0;
  Stream.CopyFrom(FStream, FStream.Size);

  // eventually skip padded zeroes
  if (cfPadSize in ChunkFlags) then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;


{ TMp4FileTypeChunk }

function TMp4FileTypeChunk.GetChunkSize: Cardinal;
begin
  Result := 8 + Length(FCompatibleBrands) * 4;
end;

class function TMp4FileTypeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'ftyp';
end;

function TMp4FileTypeChunk.GetCompatibleBrandsAsString: String;
var
  Index: Integer;
begin
  if Length(FCompatibleBrands) = 0 then
    Result := '';

  Result := FCompatibleBrands[0];

  for Index := 1 to High(FCompatibleBrands) do
    Result := Result + ', ' + FCompatibleBrands[Index];
end;

procedure TMp4FileTypeChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  CompatibleBrandCount: Integer;
begin
  inherited;

  Stream.Read(FMajorBrand, 4);
  Stream.Read(FMinorVersion, 4);
  CompatibleBrandCount := (FChunkSize - 8) div SizeOf(Integer);
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


{ TMp4MovieHeaderChunk }

constructor TMp4MovieHeaderChunk.Create;
begin
  inherited;

  FVersion := 0;
  FFlags[0] := 0;
  FFlags[1] := 0;
  FFlags[2] := 0;
  FCreationTime := SecondsBetween(Now, EncodeDateTime(1904, 1, 1, 0, 0, 0, 0));
  FModificationTime := SecondsBetween(Now, EncodeDateTime(1904, 1, 1, 0, 0, 0, 0));
  FTimeScale := $3E8;
  FDuration := $57BC0;
  FPreferredRate := $10000;
  FPreferredVolume := $100;
  FillChar(FReserved[0], 10, 0);
  FMatrixStructure[0] := $100;
  FMatrixStructure[1] := 0;
  FMatrixStructure[2] := 0;
  FMatrixStructure[3] := 0;
  FMatrixStructure[4] := $100;
  FMatrixStructure[5] := 0;
  FMatrixStructure[6] := 0;
  FMatrixStructure[7] := 0;
  FMatrixStructure[8] := $40;
  FPreviewTime := 0;
  FPreviewDuration := 0;
  FPosterTime := 0;
  FSelectionTime := 0;
  FSelectionDuration := 0;
  FCurrentTime := 0;
  FNextTrackId := $2000000;
end;

function TMp4MovieHeaderChunk.GetChunkSize: Cardinal;
begin
  Result := 100;
end;

class function TMp4MovieHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mvhd';
end;

function TMp4MovieHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FCreationTime);
end;

function TMp4MovieHeaderChunk.GetDurationAsFloat: Single;
begin
  Result :=  FDuration / TimeScale;
end;

function TMp4MovieHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FModificationTime);
end;

function TMp4MovieHeaderChunk.GetPreferredRateAsFloat: Single;
begin
  Result := FPreferredRate / $10000;
end;

function TMp4MovieHeaderChunk.GetPreferredVolumeAsFloat: Single;
begin
  Result := FPreferredVolume / $100;
end;

procedure TMp4MovieHeaderChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags[0], 3);

  FCreationTime := ReadSwappedCardinal(Stream);
  FModificationTime := ReadSwappedCardinal(Stream);
  FTimeScale := ReadSwappedCardinal(Stream);
  FDuration := ReadSwappedCardinal(Stream);
  FPreferredRate := ReadSwappedCardinal(Stream);
  FPreferredVolume := ReadSwappedWord(Stream);

  // read reserved
  Stream.Read(FReserved[0], 10);

  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    FMatrixStructure[Index] := ReadSwappedCardinal(Stream);
  FPreviewTime := ReadSwappedCardinal(Stream);
  FPreviewDuration := ReadSwappedCardinal(Stream);
  FPosterTime := ReadSwappedCardinal(Stream);
  FSelectionTime := ReadSwappedCardinal(Stream);
  FSelectionDuration := ReadSwappedCardinal(Stream);
  FCurrentTime := ReadSwappedCardinal(Stream);
  FNextTrackId := ReadSwappedCardinal(Stream);
end;

procedure TMp4MovieHeaderChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags[0], 3);

  WriteSwappedCardinal(Stream, FCreationTime);
  WriteSwappedCardinal(Stream, FModificationTime);
  WriteSwappedCardinal(Stream, FTimeScale);
  WriteSwappedCardinal(Stream, FDuration);
  WriteSwappedCardinal(Stream, FPreferredRate);
  WriteSwappedWord(Stream, FPreferredVolume);

  // Write reserved
  Stream.Write(FReserved[0], 10);

  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    WriteSwappedCardinal(Stream, FMatrixStructure[Index]);
  WriteSwappedCardinal(Stream, FPreviewTime);
  WriteSwappedCardinal(Stream, FPreviewDuration);
  WriteSwappedCardinal(Stream, FPosterTime);
  WriteSwappedCardinal(Stream, FSelectionTime);
  WriteSwappedCardinal(Stream, FSelectionDuration);
  WriteSwappedCardinal(Stream, FCurrentTime);
  WriteSwappedCardinal(Stream, FNextTrackId);
end;


{ TMp4InitialObjectDescriptorChunk }

function TMp4InitialObjectDescriptorChunk.GetChunkSize: Cardinal;
begin
  Result := 16;
end;

class function TMp4InitialObjectDescriptorChunk.GetClassChunkName: TChunkName;
begin
  Result := 'iods';
end;

procedure TMp4InitialObjectDescriptorChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags[0], 3);

  Stream.Read(FInitialObjectDescriptorTag, 1);
  Stream.Read(FExtendedDescriptorTypeTag[0], 3);
  Stream.Read(FDescriptorTypeLength, 1);
  FObjectDescriptorID := ReadSwappedWord(Stream);
  Stream.Read(FObjectDescriptorProfileLevel, 1);
  Stream.Read(FSceneProfileLevel, 1);
  Stream.Read(FAudioProfileLevel, 1);
  Stream.Read(FVideoProfileLevel, 1);
  Stream.Read(FGraphicsProfileLevel, 1);
end;

procedure TMp4InitialObjectDescriptorChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags[0], 3);

  Stream.Write(FInitialObjectDescriptorTag, 1);
  Stream.Write(FExtendedDescriptorTypeTag[0], 3);
  Stream.Write(FDescriptorTypeLength, 1);
  WriteSwappedWord(Stream, FObjectDescriptorID);
  Stream.Write(FObjectDescriptorProfileLevel, 1);
  Stream.Write(FSceneProfileLevel, 1);
  Stream.Write(FAudioProfileLevel, 1);
  Stream.Write(FVideoProfileLevel, 1);
  Stream.Write(FGraphicsProfileLevel, 1);
end;


{ TMp4TrackHeaderChunk }

function TMp4TrackHeaderChunk.GetChunkSize: Cardinal;
begin
  Result := 84;
end;

class function TMp4TrackHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'tkhd';
end;

function TMp4TrackHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FCreationTime);
end;

function TMp4TrackHeaderChunk.GetIsEnabled: Boolean;
begin
  Result := (FFlags[2] and $1) > 0;
end;

function TMp4TrackHeaderChunk.GetIsInMovie: Boolean;
begin
  Result := (FFlags[2] and $2) > 0;
end;

function TMp4TrackHeaderChunk.GetIsInPoster: Boolean;
begin
  Result := (FFlags[2] and $8) > 0;
end;

function TMp4TrackHeaderChunk.GetIsInPreview: Boolean;
begin
  Result := (FFlags[2] and $4) > 0;
end;

function TMp4TrackHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FModificationTime);
end;

function TMp4TrackHeaderChunk.GetVolumeAsFloat: Single;
begin
  Result := FVolume / $100;
end;

procedure TMp4TrackHeaderChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags[0], 3);

  FCreationTime := ReadSwappedCardinal(Stream);
  FModificationTime := ReadSwappedCardinal(Stream);

  FTrackID := ReadSwappedCardinal(Stream);
  FReservedA := ReadSwappedCardinal(Stream);
  FDuration := ReadSwappedCardinal(Stream);
  Stream.Read(FReservedB, 8);
  FLayer := ReadSwappedWord(Stream);
  FAlternateGroup := ReadSwappedWord(Stream);
  FVolume := ReadSwappedWord(Stream);
  FReservedC := ReadSwappedWord(Stream);
  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    FMatrixStructure[Index] := ReadSwappedCardinal(Stream);
  FTrackWidth := ReadSwappedCardinal(Stream);
  FTrackHeight := ReadSwappedCardinal(Stream);
end;

procedure TMp4TrackHeaderChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags[0], 3);

  WriteSwappedCardinal(Stream, FCreationTime);
  WriteSwappedCardinal(Stream, FModificationTime);

  WriteSwappedCardinal(Stream, FTrackID);
  WriteSwappedCardinal(Stream, FReservedA);
  WriteSwappedCardinal(Stream, FDuration);
  Stream.Write(FReservedB, 8);
  WriteSwappedWord(Stream, FLayer);
  WriteSwappedWord(Stream, FAlternateGroup);
  WriteSwappedWord(Stream, FVolume);
  WriteSwappedWord(Stream, FReservedC);
  for Index := Low(FMatrixStructure) to High(FMatrixStructure) do
    WriteSwappedCardinal(Stream, FMatrixStructure[Index]);
  WriteSwappedCardinal(Stream, FTrackWidth);
  WriteSwappedCardinal(Stream, FTrackHeight);
end;


{ TMp4EditListEntry }

function TMp4EditListEntry.GetMediaRateAsFloat: Single;
begin
  Result := FMediaRate / $10000;
end;

procedure TMp4EditListEntry.LoadFromStream(Stream: TStream);
begin
  FTrackDuration := ReadSwappedCardinal(Stream);
  FMediaTime := ReadSwappedCardinal(Stream);
  FMediaRate := ReadSwappedCardinal(Stream);
end;

procedure TMp4EditListEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FTrackDuration);
  WriteSwappedCardinal(Stream, FMediaTime);
  WriteSwappedCardinal(Stream, FMediaRate);
end;


{ TMp4EditListEntryList }

function TMp4EditListEntryList.Add(AChunk: TMp4EditListEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4EditListEntryList.Extract(Item: TMp4EditListEntry): TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited Extract(TObject(Item)));
end;

function TMp4EditListEntryList.First: TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited First);
end;

function TMp4EditListEntryList.GetItem(Index: Integer): TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited GetItem(Index));
end;

function TMp4EditListEntryList.IndexOf(AChunk: TMp4EditListEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4EditListEntryList.Insert(Index: Integer; AChunk: TMp4EditListEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4EditListEntryList.Last: TMp4EditListEntry;
begin
  Result := TMp4EditListEntry(inherited Last);
end;

function TMp4EditListEntryList.Remove(AChunk: TMp4EditListEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4EditListEntryList.SetItem(Index: Integer;
  AChunk: TMp4EditListEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4EditListChunk }

constructor TMp4EditListChunk.Create;
begin
  inherited;

  FList := TMp4EditListEntryList.Create;
end;

destructor TMp4EditListChunk.Destroy;
begin
  FList.Free;

  inherited;
end;

function TMp4EditListChunk.GetChunkSize: Cardinal;
begin
  Result := 8 + FList.Count * 3 * SizeOf(Cardinal);
end;

class function TMp4EditListChunk.GetClassChunkName: TChunkName;
begin
  Result := 'elst';
end;

procedure TMp4EditListChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4EditListEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags[0], 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    ListEntry := TMp4EditListEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FList.Add(ListEntry);
  end;
end;

procedure TMp4EditListChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags[0], 3);

  FNumberOfEntries := FList.Count;
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to FList.Count - 1 do
    FList[Index].SaveToStream(Stream);
end;


{ TMp4EditChunk }

constructor TMp4EditChunk.Create;
begin  inherited;

  RegisterChunkClasses([TMp4EditListChunk]);
end;

class function TMp4EditChunk.GetClassChunkName: TChunkName;
begin
  Result := 'edts';
end;


{ TMp4MediaHeaderChunk }

class function TMp4MediaHeaderChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mdhd';
end;

function TMp4MediaHeaderChunk.GetChunkSize: Cardinal;
begin
  Result := 24;
end;

function TMp4MediaHeaderChunk.GetCreationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FCreationTime);
end;

function TMp4MediaHeaderChunk.GetModificationTime: TDateTime;
begin
  Result := IncSecond(EncodeDateTime(1904, 1, 1, 0, 0, 0, 0), FModificationTime);
end;

function TMp4MediaHeaderChunk.GetDuration: Single;
begin
  Result :=  FDuration / FTimeScale;
end;

function TMp4MediaHeaderChunk.GetTimeScale: Single;
begin
  Result :=  FTimeScale;
end;


procedure TMp4MediaHeaderChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);
  FCreationTime := ReadSwappedCardinal(Stream);
  FModificationTime := ReadSwappedCardinal(Stream);
  FTimeScale := ReadSwappedCardinal(Stream);
  FDuration := ReadSwappedCardinal(Stream);
  FLanguage := ReadSwappedWord(Stream);
  FQuality := ReadSwappedWord(Stream);
end;

procedure TMp4MediaHeaderChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);
  WriteSwappedCardinal(Stream, FCreationTime);
  WriteSwappedCardinal(Stream, FModificationTime);
  WriteSwappedCardinal(Stream, FTimeScale);
  WriteSwappedCardinal(Stream, FDuration);
  WriteSwappedWord(Stream, FLanguage);
  WriteSwappedWord(Stream, FQuality);
end;


{ TMp4HandlerReferenceChunk }

class function TMp4HandlerReferenceChunk.GetClassChunkName: TChunkName;
begin
  Result := 'hdlr';
end;

function TMp4HandlerReferenceChunk.GetChunkSize: Cardinal;
begin
  Result := 24 + Length(FComponentName);
end;

procedure TMp4HandlerReferenceChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  Stream.Read(FComponentType, 4);
  Stream.Read(FComponentSubType, 4);
  Stream.Read(FComponentManufacturer, 4);
  Stream.Read(FComponentFlags, 4);
  Stream.Read(FComponentFlagMask, 4);
  SetLength(FComponentName, FChunkSize - 24);
  Stream.Read(FComponentName[1], FChunkSize - 24);
end;

procedure TMp4HandlerReferenceChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  Stream.Write(FComponentType, 4);
  Stream.Write(FComponentSubType, 4);
  Stream.Write(FComponentManufacturer, 4);
  Stream.Write(FComponentFlags, 4);
  Stream.Write(FComponentFlagMask, 4);
  Stream.Write(FComponentName[1], Length(FComponentName));
end;


{ TMp4VideoMediaInformationChunk }

function TMp4VideoMediaInformationChunk.GetChunkSize: Cardinal;
begin
  Result := 9;
end;

class function TMp4VideoMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'vmhd'
end;

function TMp4VideoMediaInformationChunk.GetOpColor(Index: Byte): Word;
begin
  if not (Index in [0..2]) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  Result := FOpColor[Index];
end;

procedure TMp4VideoMediaInformationChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);
  FGraphicsMode := ReadSwappedWord(Stream);
  Stream.Read(FOpColor, 3);
end;

procedure TMp4VideoMediaInformationChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);
  WriteSwappedWord(Stream, FGraphicsMode);
  Stream.Write(FOpColor, 3);
end;


{ TMp4SoundMediaInformationChunk }

class function TMp4SoundMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'smhd'
end;

function TMp4SoundMediaInformationChunk.GetChunkSize: Cardinal;
begin
  Result := 8;
end;

procedure TMp4SoundMediaInformationChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FBalance := ReadSwappedWord(Stream);
  FReserved := ReadSwappedWord(Stream);
end;

procedure TMp4SoundMediaInformationChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedWord(Stream, FBalance);
  WriteSwappedWord(Stream, FReserved);
end;


{ TMp4MediaInformationChunk }

constructor TMp4MediaInformationChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4VideoMediaInformationChunk,
    TMp4SoundMediaInformationChunk, TMp4BaseMediaInformationChunk,
    TMp4HandlerReferenceChunk, TMp4DataInformationChunk, TMp4SampleTableChunk]);
end;

class function TMp4MediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'minf';
end;


{ TMp4BaseMediaInformationChunk }

function TMp4BaseMediaInformationChunk.GetChunkSize: Cardinal;
begin
  Result := 13;
end;

class function TMp4BaseMediaInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'gmin';
end;

function TMp4BaseMediaInformationChunk.GetOpColor(Index: Byte): Word;
begin
  if not (Index in [0..2]) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  Result := FOpColor[Index];
end;

procedure TMp4BaseMediaInformationChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);
  FGraphicsMode := ReadSwappedWord(Stream);
  Stream.Read(FOpColor, 3);
  FBalance := ReadSwappedWord(Stream);
  FReserved := ReadSwappedWord(Stream);
end;

procedure TMp4BaseMediaInformationChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedWord(Stream, FGraphicsMode);
  Stream.Write(FOpColor, 3);

  WriteSwappedWord(Stream, FBalance);
  WriteSwappedWord(Stream, FReserved);
end;


{ TMp4DataReferenceUrlChunk }

function TMp4DataReferenceUrlChunk.GetChunkSize: Cardinal;
begin
  Result := 4;
end;

class function TMp4DataReferenceUrlChunk.GetClassChunkName: TChunkName;
begin
  Result := 'url ';
end;

procedure TMp4DataReferenceUrlChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  // read version
  Stream.Read(FVersion, 1);

  // read flags
  Stream.Read(FFlags, 3);
end;

procedure TMp4DataReferenceUrlChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  // write version
  Stream.Write(FVersion, 1);

  // write flags
  Stream.Write(FFlags, 3);
end;


{ TMp4DataReferenceChunk }

class function TMp4DataReferenceChunk.GetClassChunkName: TChunkName;
begin
  Result := 'dref';
end;

constructor TMp4DataReferenceChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4DataReferenceUrlChunk]);
end;

function TMp4DataReferenceChunk.GetChunkSize: Cardinal;
begin
  Result := inherited GetChunkSize + 8;
end;

procedure TMp4DataReferenceChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  SubChunkName: TChunkName;
  SubChunkSize: Cardinal;
begin
  // read chunk size
  FChunkSize := ReadSwappedCardinal(Stream);

  // read chunk name
  Stream.Read(FChunkName, 4);

  // read version
  Stream.Read(FVersion, 1);

  // read flags
  Stream.Read(FFlags, 3);

  // read number of entries
  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    // read chunk size
    SubChunkSize := ReadSwappedCardinal(Stream);

    // read chunk name
    Stream.Read(SubChunkName, 4);

    Stream.Position := Stream.Position - 8;
    ConvertStreamToChunk(GetChunkClass(SubChunkName), Stream);
  end;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4DataReferenceChunk.SaveToStream(Stream: TStream);
var
  i: Integer;
  ChunkSize: Cardinal;
begin
  // write chunk size
  FChunkSize := GetChunkSize;
  ChunkSize := FChunkSize + 8;
  WriteSwappedCardinal(Stream, ChunkSize);

  // write chunk name
  Stream.Write(FChunkName, 4);

  // write version
  Stream.Write(FVersion, 1);

  // write flags
  Stream.Write(FFlags, 3);

  // write number of entries
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for i := 0 to FChunkList.Count - 1 do
    FChunkList[i].SaveToStream(Stream);

  // insert pad byte if necessary
  if cfPadSize in ChunkFlags then
    Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TMp4DataInformationChunk }

constructor TMp4DataInformationChunk.Create;
begin
  inherited;

  RegisterChunkClass(TMp4DataReferenceChunk);
end;

class function TMp4DataInformationChunk.GetClassChunkName: TChunkName;
begin
  Result := 'dinf';
end;


{ TMp4ElementaryStreamDescriptorChunk }

constructor TMp4ElementaryStreamDescriptorChunk.Create;
begin
  inherited;

  FElementaryStreamDescriptorTag := 3;
  FExtendedDescriptorTypeTag[0] := 128;
  FExtendedDescriptorTypeTag[1] := 128;
  FExtendedDescriptorTypeTag[2] := 128;
  FStreamPriority := 16;
  FDecoderConfigDescriptor := 4;
  FExtendedDecoderConfigDescriptor[0] := 128;
  FExtendedDecoderConfigDescriptor[1] := 128;
  FExtendedDecoderConfigDescriptor[2] := 128;
  FDecoderSpecificTypeValue := 5;
  FExtendedDecoderSpecificTypeValue[0] := 128;
  FExtendedDecoderSpecificTypeValue[1] := 128;
  FExtendedDecoderSpecificTypeValue[2] := 128;
  FConfigDescriptorType := 6;

  FDecoderSpecificData := TMemoryStream.Create;
end;

destructor TMp4ElementaryStreamDescriptorChunk.Destroy;
begin
  FDecoderSpecificData.Free;

  inherited;
end;

function TMp4ElementaryStreamDescriptorChunk.GetChunkSize: Cardinal;
begin
  Result := 35 + FDecoderSpecificData.Size + 6;
end;

class function TMp4ElementaryStreamDescriptorChunk.GetClassChunkName: TChunkName;
begin
  Result := 'esds';
end;

procedure TMp4ElementaryStreamDescriptorChunk.LoadFromStream(Stream: TStream);
var
  Value: Byte;
begin
  inherited;

  // read version
  Stream.Read(FVersion, 1);

  // read flags
  Stream.Read(FFlags, 3);

  // read elementary stream descriptor tag
  Stream.Read(FElementaryStreamDescriptorTag, 1);
  Stream.Read(FExtendedDescriptorTypeTag[0], 3);

  Stream.Read(FDescriptorTypeLength, 1);
  FElementaryDescriptorID := ReadSwappedWord(Stream);

  Stream.Read(FStreamPriority, 1);
  Stream.Read(FDecoderConfigDescriptor, 1);
  Stream.Read(FExtendedDecoderConfigDescriptor[0], 3);
  Stream.Read(FDecoderConfigDescriptorTypeLength, 1);

  Stream.Read(FObjectTypeId, 1);
  Stream.Read(Value, 1);
  FStreamType := TMp4StreamTypeID(Value shr 2);
  FUpstreamFlag := (Value and 2) <> 0;

  // read buffer size
  Stream.Read(Value, 1);
  FBufferSize := Value shl 16;
  Stream.Read(Value, 1);
  FBufferSize := FBufferSize or (Value shl 8);
  Stream.Read(Value, 1);
  FBufferSize := FBufferSize or Value;

  FMaximumBitRate := ReadSwappedCardinal(Stream);
  FAverageBitRate := ReadSwappedCardinal(Stream);

  Stream.Read(FDecoderSpecificTypeValue, 1);
  Stream.Read(FExtendedDecoderSpecificTypeValue[0], 1);
  Stream.Read(FExtendedDecoderSpecificTypeValue[1], 1);
  Stream.Read(FExtendedDecoderSpecificTypeValue[2], 1);

  Stream.Read(FDecoderSpecificTypeLength, 1);

  FDecoderSpecificData.CopyFrom(Stream, FDecoderSpecificTypeLength);

  Stream.Read(FConfigDescriptorType, 1);
  Stream.Read(FExtendedConfigDescriptorType[0], 1);
  Stream.Read(FExtendedConfigDescriptorType[1], 1);
  Stream.Read(FExtendedConfigDescriptorType[2], 1);
  Stream.Read(FConfigDescriptorLength, 1);
  Stream.Read(FConfigValue, 1);
end;

procedure TMp4ElementaryStreamDescriptorChunk.SaveToStream(Stream: TStream);
var
  Value: Byte;
begin
  inherited;

  // write version
  Stream.Write(FVersion, 1);

  // write flags
  Stream.Write(FFlags, 3);

  // write elementary stream descriptor tag
  Stream.Write(FElementaryStreamDescriptorTag, 1);
  Stream.Write(FExtendedDescriptorTypeTag[0], 3);

  Stream.Write(FDescriptorTypeLength, 1);
  WriteSwappedWord(Stream, FElementaryDescriptorID);

  Stream.Write(FStreamPriority, 1);
  Stream.Write(FDecoderConfigDescriptor, 1);
  Stream.Write(FExtendedDecoderConfigDescriptor[0], 3);
  Stream.Write(FDecoderConfigDescriptorTypeLength, 1);

  Stream.Write(FObjectTypeId, 1);
  Value := (Byte(FStreamType) shl 2) or (Byte(FUpstreamFlag) shl 1) or 1;
  Stream.Write(Value, 1);

  // write buffer size
  Value := (FBufferSize shr 16) and $ff;
  Stream.Write(Value, 1);
  Value := (FBufferSize shr 8) and $ff;
  Stream.Write(Value, 1);
  Value := FBufferSize and $ff;
  Stream.Write(Value, 1);

  WriteSwappedCardinal(Stream, FMaximumBitRate);
  WriteSwappedCardinal(Stream, FAverageBitRate);

  Stream.Write(FDecoderSpecificTypeValue, 1);
  Stream.Write(FExtendedDecoderSpecificTypeValue[0], 1);
  Stream.Write(FExtendedDecoderSpecificTypeValue[1], 1);
  Stream.Write(FExtendedDecoderSpecificTypeValue[2], 1);

  Stream.Write(FDecoderSpecificTypeLength, 1);

  FDecoderSpecificData.Position := 0;
  Stream.CopyFrom(FDecoderSpecificData, FDecoderSpecificData.Size);

  Stream.Write(FConfigDescriptorType, 1);
  Stream.Write(FExtendedConfigDescriptorType[0], 1);
  Stream.Write(FExtendedConfigDescriptorType[1], 1);
  Stream.Write(FExtendedConfigDescriptorType[2], 1);
  Stream.Write(FConfigDescriptorLength, 1);
  Stream.Write(FConfigValue, 1);
end;


{ TMp4Mp4aChunk }

constructor TMp4Mp4aChunk.Create;
begin
  inherited;

  FillChar(FReserved[0], 6, 0);

  RegisterChunkClasses([TMp4ElementaryStreamDescriptorChunk]);
end;

function TMp4Mp4aChunk.GetChunkSize: Cardinal;
begin
  Result := inherited GetChunkSize + 28;
end;

class function TMp4Mp4aChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mp4a';
end;

function TMp4Mp4aChunk.GetSampleRate: Single;
begin
  Result := FSampleRate / $10000;
end;

procedure TMp4Mp4aChunk.LoadFromStream(Stream: TStream);
var
  ChunkEnd: Cardinal;
  SubChunkName: TChunkName;
begin
  // read chunk size and determine chunk end position
  FChunkSize := ReadSwappedCardinal(Stream) - 8;
  ChunkEnd := Stream.Position + FChunkSize + 4;

  // read chunk data
  Stream.Read(FChunkName, 4);

  // read reserved data
  Stream.Read(FReserved, 6);

  // read data reference index
  FDataReferenceIndex := ReadSwappedWord(Stream);

  // read encoding information
  FEncodingVersionLevel := ReadSwappedWord(Stream);
  FEncodingRevisionLevel := ReadSwappedWord(Stream);
  Stream.Read(FEncodingVendor, SizeOf(TChunkName));

  // read audio information
  FChannelCount := ReadSwappedWord(Stream);
  FSampleSize := ReadSwappedWord(Stream);
  Stream.Read(FAudioCompressionId, SizeOf(Word));
  Stream.Read(FAudioPacketSize, SizeOf(Word));
  FSampleRate := ReadSwappedCardinal(Stream);

  // read sub chunks
  with Stream do
  begin
    Assert(ChunkEnd <= Stream.Size);
    while Position < ChunkEnd do
    begin
      Position := Position + 4;
      Read(SubChunkName, 4);
      Position := Position - 8;
      ConvertStreamToChunk(GetChunkClass(SubChunkName), Stream);
    end;
    if Position <> ChunkEnd then
      Position := ChunkEnd;

    // eventually skip padded zeroes
    if cfPadSize in ChunkFlags then
      Position := Position + CalculateZeroPad;
  end;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4Mp4aChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
  ChunkSize: Cardinal;
begin
  // write chunk size
  FChunkSize := GetChunkSize;
  ChunkSize := FChunkSize + 8;
  WriteSwappedCardinal(Stream, ChunkSize);

  // write chunk name
  Stream.Write(FChunkName, 4);

  // write reserved data
  Stream.Write(FReserved, 6);

  // write data reference
  WriteSwappedWord(Stream, FDataReferenceIndex);

  // write encoding information
  WriteSwappedWord(Stream, FEncodingVersionLevel);
  WriteSwappedWord(Stream, FEncodingRevisionLevel);
  Stream.Write(FEncodingVendor, 4);

  // write encoding information
  WriteSwappedWord(Stream, FChannelCount);
  WriteSwappedWord(Stream, FSampleSize);
  Stream.Write(FAudioCompressionId, SizeOf(Word));
  Stream.Write(FAudioPacketSize, SizeOf(Word));
  WriteSwappedCardinal(Stream, FSampleRate);

  for Index := 0 to FChunkList.Count - 1 do
    FChunkList[Index].SaveToStream(Stream);

  // insert pad byte if necessary
  if cfPadSize in ChunkFlags then
    Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TMp4SampleDescriptionChunk }

constructor TMp4SampleDescriptionChunk.Create;
begin
  inherited;

  RegisterChunkClass(TMp4Mp4aChunk);
end;

function TMp4SampleDescriptionChunk.GetChunkSize: Cardinal;
begin
  Result := inherited GetChunkSize + 8;
end;

class function TMp4SampleDescriptionChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stsd';
end;

procedure TMp4SampleDescriptionChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  SubChunkName: TChunkName;
  SubChunkSize: Cardinal;
begin
  // read chunk size
  FChunkSize := ReadSwappedCardinal(Stream);

  // read chunk name
  Stream.Read(FChunkName, 4);

  // read version and flags
  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);
  for Index := 0 to FNumberOfEntries - 1 do
  begin
    // read chunk size
    SubChunkSize := ReadSwappedCardinal(Stream);

    // read chunk name
    Stream.Read(SubChunkName, 4);

    Stream.Position := Stream.Position - 8;
    ConvertStreamToChunk(GetChunkClass(SubChunkName), Stream);
  end;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4SampleDescriptionChunk.SaveToStream(Stream: TStream);
var
  Value: Cardinal;
  ChunkSize: Cardinal;
  i: Integer;
begin
  // write chunk size
  FChunkSize := GetChunkSize;
  ChunkSize := FChunkSize + 8;
  WriteSwappedCardinal(Stream, ChunkSize);

  // write chunk name
  Stream.Write(FChunkName, 4);

  // write version and flags
  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  // write number of entries
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for i := 0 to FChunkList.Count - 1 do
    FChunkList[i].SaveToStream(Stream);

  // insert pad byte if necessary
  if cfPadSize in ChunkFlags then
    Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TMp4SampleTableEntry }

procedure TMp4SampleTableEntry.LoadFromStream(Stream: TStream);
begin
  FSampleCount := ReadSwappedCardinal(Stream);
  FSampleDuration := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleTableEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FSampleCount);
  WriteSwappedCardinal(Stream, FSampleDuration);
end;


{ TMp4SampleTableEntryList }

function TMp4SampleTableEntryList.Add(AChunk: TMp4SampleTableEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4SampleTableEntryList.Extract(
  Item: TMp4SampleTableEntry): TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited Extract(TObject(Item)));
end;

function TMp4SampleTableEntryList.First: TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited First);
end;

function TMp4SampleTableEntryList.GetItem(Index: Integer): TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited GetItem(Index));
end;

function TMp4SampleTableEntryList.IndexOf(AChunk: TMp4SampleTableEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4SampleTableEntryList.Insert(Index: Integer;
  AChunk: TMp4SampleTableEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4SampleTableEntryList.Last: TMp4SampleTableEntry;
begin
  Result := TMp4SampleTableEntry(inherited Last);
end;

function TMp4SampleTableEntryList.Remove(AChunk: TMp4SampleTableEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4SampleTableEntryList.SetItem(Index: Integer;
  AChunk: TMp4SampleTableEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4TimeToSampleChunk }

constructor TMp4TimeToSampleChunk.Create;
begin
  inherited;

  FSampleTable := TMp4SampleTableEntryList.Create;
end;

destructor TMp4TimeToSampleChunk.Destroy;
begin
  FSampleTable.Free;

  inherited;
end;

function TMp4TimeToSampleChunk.GetChunkSize: Cardinal;
begin
  Result := 8 + FSampleTable.Count * 8;
end;

class function TMp4TimeToSampleChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stts';
end;

procedure TMp4TimeToSampleChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4SampleTableEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    ListEntry := TMp4SampleTableEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FSampleTable.Add(ListEntry);
  end;
end;

procedure TMp4TimeToSampleChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to FSampleTable.Count - 1 do
    FSampleTable[Index].SaveToStream(Stream);
end;


{ TMp4SyncSampleChunk }

constructor TMp4SyncSampleChunk.Create;
begin
  inherited;

  FNumberOfEntries := 0;
end;

function TMp4SyncSampleChunk.GetChunkSize: Cardinal;
begin
  Result := 8 + Length(FSyncSampleTable) * 4;
end;

class function TMp4SyncSampleChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stss';
end;

function TMp4SyncSampleChunk.GetCount: Cardinal;
begin
  Result := Length(FSyncSampleTable);
end;

function TMp4SyncSampleChunk.GetSyncSample(Index: Integer): Cardinal;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  Result := FSyncSampleTable[Index];
end;

procedure TMp4SyncSampleChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  SetLength(FSyncSampleTable, FNumberOfEntries);
  for Index := 0 to FNumberOfEntries - 1 do
    FSyncSampleTable[Index] := ReadSwappedCardinal(Stream);
end;

procedure TMp4SyncSampleChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  FNumberOfEntries := Length(FSyncSampleTable);
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to Length(FSyncSampleTable) - 1 do
    WriteSwappedCardinal(Stream, FSyncSampleTable[Index]);
end;


{ TMp4SampleToChunkEntry }

procedure TMp4SampleToChunkEntry.LoadFromStream(Stream: TStream);
begin
  FFirstChunk := ReadSwappedCardinal(Stream);
  FSamplesPerChunk := ReadSwappedCardinal(Stream);
  FSampleDescription := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleToChunkEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FFirstChunk);
  WriteSwappedCardinal(Stream, FSamplesPerChunk);
  WriteSwappedCardinal(Stream, FSampleDescription);
end;


{ TMp4SampleToChunkEntryList }

function TMp4SampleToChunkEntryList.Add(AChunk: TMp4SampleToChunkEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4SampleToChunkEntryList.Extract(
  Item: TMp4SampleToChunkEntry): TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited Extract(TObject(Item)));
end;

function TMp4SampleToChunkEntryList.First: TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited First);
end;

function TMp4SampleToChunkEntryList.GetItem(Index: Integer): TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited GetItem(Index));
end;

function TMp4SampleToChunkEntryList.IndexOf(AChunk: TMp4SampleToChunkEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4SampleToChunkEntryList.Insert(Index: Integer;
  AChunk: TMp4SampleToChunkEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4SampleToChunkEntryList.Last: TMp4SampleToChunkEntry;
begin
  Result := TMp4SampleToChunkEntry(inherited Last);
end;

function TMp4SampleToChunkEntryList.Remove(AChunk: TMp4SampleToChunkEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4SampleToChunkEntryList.SetItem(Index: Integer;
  AChunk: TMp4SampleToChunkEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4SampleToChunkChunk }

constructor TMp4SampleToChunkChunk.Create;
begin
  inherited;

  FSampleToChunkTable := TMp4SampleToChunkEntryList.Create;
end;

destructor TMp4SampleToChunkChunk.Destroy;
begin
  FSampleToChunkTable.Free;

  inherited;
end;

function TMp4SampleToChunkChunk.GetChunkSize: Cardinal;
begin
  Result := 8 + FSampleToChunkTable.Count * 12;
end;

class function TMp4SampleToChunkChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stsc';
end;

procedure TMp4SampleToChunkChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4SampleToChunkEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  for Index := 0 to FNumberOfEntries - 1 do
  begin
    ListEntry := TMp4SampleToChunkEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FSampleToChunkTable.Add(ListEntry);
  end;
end;

procedure TMp4SampleToChunkChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  FNumberOfEntries := FSampleToChunkTable.Count;
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to FSampleToChunkTable.Count - 1 do
    FSampleToChunkTable[Index].SaveToStream(Stream);
end;


{ TMp4SampleSizeChunk }

constructor TMp4SampleSizeChunk.Create;
begin
  inherited;
end;

function TMp4SampleSizeChunk.GetChunkSize: Cardinal;
begin
  Result := 12 + Length(FSampleSizes) * SizeOf(Cardinal);
end;

class function TMp4SampleSizeChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stsz';
end;

function TMp4SampleSizeChunk.GetCount: Cardinal;
begin
  Result := Length(FSampleSizes);
end;

function TMp4SampleSizeChunk.GetSampleSize(Index: Integer): Cardinal;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

  Result := FSampleSizes[Index];
end;

procedure TMp4SampleSizeChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FSampleSize := ReadSwappedCardinal(Stream);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  SetLength(FSampleSizes, FNumberOfEntries);
  for Index := 0 to FNumberOfEntries - 1 do
    FSampleSizes[Index] := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleSizeChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  WriteSwappedCardinal(Stream, FSampleSize);

  FNumberOfEntries := Length(FSampleSizes);
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  for Index := 0 to Length(FSampleSizes) - 1 do
    WriteSwappedCardinal(Stream, FSampleSizes[Index]);
end;


{ TMp4ChunkOffsetChunk }

constructor TMp4ChunkOffsetChunk.Create;
begin
  inherited;
end;

class function TMp4ChunkOffsetChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stco';
end;

function TMp4ChunkOffsetChunk.GetChunkOffset(Index: Integer): Int64;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('Index out of bounds');

  Result := FChunkOffsetTable[Index];
end;

function TMp4ChunkOffsetChunk.GetChunkSize: Cardinal;
begin
  Result := 8 + Length(FChunkOffsetTable) * 4;
end;

function TMp4ChunkOffsetChunk.GetCount: Cardinal;
begin
  Result := Length(FChunkOffsetTable);
end;

procedure TMp4ChunkOffsetChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  FNumberOfEntries := ReadSwappedCardinal(Stream);

  // TODO: check for 32/64-bit
  SetLength(FChunkOffsetTable, FNumberOfEntries);
  for Index := 0 to FNumberOfEntries - 1 do
    FChunkOffsetTable[Index] := ReadSwappedCardinal(Stream);
end;

procedure TMp4ChunkOffsetChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  FNumberOfEntries := Length(FChunkOffsetTable);
  WriteSwappedCardinal(Stream, FNumberOfEntries);

  // TODO: check for 32/64-bit
  for Index := 0 to Length(FChunkOffsetTable) - 1 do
    WriteSwappedCardinal(Stream, FChunkOffsetTable[Index]);
end;


{ TMp4SampleGroupDescriptionChunk }

constructor TMp4SampleGroupDescriptionChunk.Create;
begin
  inherited;

  FGroupingType := 'roll';
  FDefaultLength := 2;
end;

class function TMp4SampleGroupDescriptionChunk.GetClassChunkName: TChunkName;
begin
  Result := 'sgpd';
end;

function TMp4SampleGroupDescriptionChunk.GetChunkSize: Cardinal;
begin
  Result := 16 + Length(FPayload) * SizeOf(Word);
end;

procedure TMp4SampleGroupDescriptionChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  // read version & flags
  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  // read grouping table
  Stream.Read(FGroupingType, 4);

  // read default length
  FDefaultLength := ReadSwappedCardinal(Stream);

  FEntryCount := ReadSwappedCardinal(Stream);

  SetLength(FPayload, 1);
  for Index := 0 to FEntryCount - 1 do
    FPayload[Index] := ReadSwappedWord(Stream);
end;

procedure TMp4SampleGroupDescriptionChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  // write version & flags
  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  // write grouping table
  Stream.Write(FGroupingType, 4);

  // write default length
  WriteSwappedCardinal(Stream, FDefaultLength);

  FEntryCount := Length(FPayload);
  WriteSwappedCardinal(Stream, FEntryCount);

  for Index := 0 to Length(FPayload) - 1 do
    WriteSwappedWord(Stream, FPayload[Index]);
end;


{ TMp4SampleToGroupEntry }

procedure TMp4SampleToGroupEntry.LoadFromStream(Stream: TStream);
begin
  FSampleCount := ReadSwappedCardinal(Stream);
  FGroupDescriptionIndex := ReadSwappedCardinal(Stream);
end;

procedure TMp4SampleToGroupEntry.SaveToStream(Stream: TStream);
begin
  WriteSwappedCardinal(Stream, FSampleCount);
  WriteSwappedCardinal(Stream, FGroupDescriptionIndex);
end;


{ TMp4SampleToGroupEntryList }

function TMp4SampleToGroupEntryList.Add(AChunk: TMp4SampleToGroupEntry): Integer;
begin
  Result := inherited Add(TObject(AChunk));
end;

function TMp4SampleToGroupEntryList.Extract(
  Item: TMp4SampleToGroupEntry): TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited Extract(TObject(Item)));
end;

function TMp4SampleToGroupEntryList.First: TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited First);
end;

function TMp4SampleToGroupEntryList.GetItem(Index: Integer): TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited GetItem(Index));
end;

function TMp4SampleToGroupEntryList.IndexOf(AChunk: TMp4SampleToGroupEntry): Integer;
begin
  Result := inherited IndexOf(TObject(AChunk));
end;

procedure TMp4SampleToGroupEntryList.Insert(Index: Integer;
  AChunk: TMp4SampleToGroupEntry);
begin
  inherited Insert(Index, TObject(AChunk));
end;

function TMp4SampleToGroupEntryList.Last: TMp4SampleToGroupEntry;
begin
  Result := TMp4SampleToGroupEntry(inherited Last);
end;

function TMp4SampleToGroupEntryList.Remove(AChunk: TMp4SampleToGroupEntry): Integer;
begin
  Result := inherited Remove(TObject(AChunk));
end;

procedure TMp4SampleToGroupEntryList.SetItem(Index: Integer;
  AChunk: TMp4SampleToGroupEntry);
begin
  inherited SetItem(Index, TObject(AChunk));
end;


{ TMp4SampleToGroupChunk }

constructor TMp4SampleToGroupChunk.Create;
begin
  inherited;

  FGroupingType := 'roll';
  FTableData := TMp4SampleToGroupEntryList.Create;
end;

destructor TMp4SampleToGroupChunk.Destroy;
begin
  FTableData.Free;

  inherited;
end;

function TMp4SampleToGroupChunk.GetChunkSize: Cardinal;
begin
  Result := 12 + FTableData.Count * 2 * SizeOf(Cardinal);
end;

class function TMp4SampleToGroupChunk.GetClassChunkName: TChunkName;
begin
  Result := 'sbgp';
end;

procedure TMp4SampleToGroupChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  ListEntry: TMp4SampleToGroupEntry;
begin
  inherited;

  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);

  Stream.Read(FGroupingType, 4);

  FEntryCount := ReadSwappedCardinal(Stream);

  for Index := 0 to FEntryCount - 1 do
  begin
    ListEntry := TMp4SampleToGroupEntry.Create;
    ListEntry.LoadFromStream(Stream);
    FTableData.Add(ListEntry);
  end;
end;

procedure TMp4SampleToGroupChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  inherited;

  Stream.Write(FVersion, 1);
  Stream.Write(FFlags, 3);

  Stream.Write(FGroupingType, 4);

  FEntryCount := FTableData.Count;
  WriteSwappedCardinal(Stream, FEntryCount);

  for Index := 0 to FTableData.Count - 1 do
    FTableData[Index].SaveToStream(Stream);
end;


{ TMp4CompactSampleToGroupChunk }

constructor TMp4CompactSampleToGroupChunk.Create;
begin
  inherited;

  FGroupingType := 'roll';
end;

function TMp4CompactSampleToGroupChunk.GetChunkSize: Cardinal;
begin
  Result := 12 + FPatternLengthSize + FSampleCountSize + ((FNumberOfFramesFirstPeriod + 1) div 2);
end;

class function TMp4CompactSampleToGroupChunk.GetClassChunkName: TChunkName;
begin
  Result := 'csgp';
end;

procedure TMp4CompactSampleToGroupChunk.LoadFromStream(Stream: TStream);
var
  Index: Integer;
  Value: Byte;
begin
  inherited;

  // read version and flags (just a wild guess)
  Stream.Read(FVersion, 1);
  Stream.Read(FFlags, 3);
  FPatternLengthSize := (FFlags[2] shr 4) and 3;
  FSampleCountSize := (FFlags[2] shr 2) and 3;

  // read grouping type and a yet unknown value
  Stream.Read(FGroupingType, 4);
  Stream.Read(FUnknown, 4);

  if FPatternLengthSize > 1 then
    FNumberOfFramesFirstPeriod := ReadSwappedWord(Stream)
  else
    Stream.Read(FNumberOfFramesFirstPeriod, 1);

  if FSampleCountSize > 2 then
    FNumberOfFrames := ReadSwappedCardinal(Stream)
  else
  if FSampleCountSize = 2 then
    FNumberOfFrames := ReadSwappedWord(Stream)
  else
    Stream.Read(FNumberOfFrames, 1);

  FEntryCount := FNumberOfFramesFirstPeriod;
  SetLength(FPayload, FEntryCount);
  for Index := 0 to ((FNumberOfFramesFirstPeriod + 1) div 2) - 1 do
  begin
    Stream.Read(Value, 1);
    FPayload[Index * 2 + 0] := Value shr 4;
    FPayload[Index * 2 + 1] := Value and $F;
  end;
end;

procedure TMp4CompactSampleToGroupChunk.SaveToStream(Stream: TStream);
var
  Index: Integer;
  Value: Byte;
begin
  inherited;

  // write version and flags (just a wild guess)
  Stream.Write(FVersion, 1);
  FFlags[2] := ((FPatternLengthSize and 3) shl 4) or ((FSampleCountSize and 3) shl 2);
  Stream.Write(FFlags, 3);

  // write grouping type and a yet unknown value
  Stream.Write(FGroupingType, 4);
  Stream.Write(FUnknown, 4);

  if FPatternLengthSize > 1 then
    WriteSwappedWord(Stream, FNumberOfFramesFirstPeriod)
  else
    Stream.Write(FNumberOfFramesFirstPeriod, 1);

  if FSampleCountSize > 2 then
    WriteSwappedCardinal(Stream, FNumberOfFrames)
  else
  if FSampleCountSize = 2 then
    WriteSwappedWord(Stream, FNumberOfFrames)
  else
    Stream.Write(FNumberOfFrames, 1);

  // write payload
  for Index := 0 to ((FNumberOfFramesFirstPeriod + 1) div 2) - 1 do
  begin
    Value :=
      ((FPayload[Index * 2 + 0] and $F) shl 4) or
       (FPayload[Index * 2 + 1] and $F);
    Stream.Write(Value, 1);
  end;
end;


{ TMp4SampleTableChunk }

constructor TMp4SampleTableChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4SyncSampleChunk, TMp4SampleDescriptionChunk,
    TMp4TimeToSampleChunk, TMp4SampleToChunkChunk, TMp4SampleSizeChunk,
    TMp4ChunkOffsetChunk, TMp4SampleGroupDescriptionChunk,
    TMp4SampleToGroupChunk, TMp4CompactSampleToGroupChunk]);
end;

class function TMp4SampleTableChunk.GetClassChunkName: TChunkName;
begin
  Result := 'stbl';
end;

procedure TMp4SampleTableChunk.SaveToStream(Stream: TStream);
var
  sz: Integer;
  Index: Integer;
begin
  inherited;

  sz := 0;
  for Index := 0 to Count - 1 do
    sz := sz + SubChunk[Index].ChunkSize;

end;



{ TMp4MediaChunk }

constructor TMp4MediaChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4MediaHeaderChunk, TMp4HandlerReferenceChunk,
    TMp4MediaInformationChunk]);
end;

class function TMp4MediaChunk.GetClassChunkName: TChunkName;
begin
  Result := 'mdia';
end;


{ TMp4TrackChunk }

constructor TMp4TrackChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4TrackHeaderChunk, TMp4EditChunk, TMp4MediaChunk]);
end;

class function TMp4TrackChunk.GetClassChunkName: TChunkName;
begin
  Result := 'trak';
end;


{ TMp4UserDataChunk }

constructor TMp4UserDataChunk.Create;
begin
  inherited;

  RegisterChunkClass(TMp4MetaChunk);
end;

class function TMp4UserDataChunk.GetClassChunkName: TChunkName;
begin
  Result := 'udta';
end;


{ TMp4DataChunk }

function TMp4DataChunk.GetChunkSize: Cardinal;
begin
  Result := 8;
end;

class function TMp4DataChunk.GetClassChunkName: TChunkName;
begin
  Result := 'data';
end;

procedure TMp4DataChunk.LoadFromStream(Stream: TStream);
begin
  inherited;

  // read type
  FType := ReadSwappedCardinal(Stream);

  // read locale
  FLocale := ReadSwappedCardinal(Stream);

  SetLength(FData, FChunkSize - 8);
  Stream.Read(FData[1], FChunkSize - 8);

  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4DataChunk.SaveToStream(Stream: TStream);
begin
  inherited;

  // read type
  Stream.Write(FType, 4);

  // read locale
  Stream.Write(FLocale, 4);

  Stream.Write(FData[1], Length(FData));
end;


{ TMp4CustomItemListChunk }

constructor TMp4CustomItemListChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4DataChunk]);
end;


destructor TMp4CustomItemListChunk.Destroy;
begin

  inherited;
end;

{ TMp4ItemListArtChunk }

class function TMp4ItemListArtChunk.GetClassChunkName: TChunkName;
begin
  Result := #$A9+'ART';
end;


{ TMp4ItemListTooChunk }

class function TMp4ItemListTooChunk.GetClassChunkName: TChunkName;
begin
  Result := #$A9+'too';
end;


{ TMp4ItemListChunk }

constructor TMp4ItemListChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4ItemListArtChunk, TMp4ItemListTooChunk]);
end;

class function TMp4ItemListChunk.GetClassChunkName: TChunkName;
begin
  Result := 'ilst';
end;


{ TMp4MetaChunk }

constructor TMp4MetaChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4HandlerReferenceChunk, TMp4ItemListChunk]);
end;

class function TMp4MetaChunk.GetClassChunkName: TChunkName;
begin
  Result := 'meta';
end;

function TMp4MetaChunk.GetChunkSize: Cardinal;
begin
  Result := inherited GetChunkSize + 4;
end;

procedure TMp4MetaChunk.LoadFromStream(Stream: TStream);
var
  ChunkEnd: Cardinal;
  SubChunkName: TChunkName;
begin
  // read chunk size
  FChunkSize := ReadSwappedCardinal(Stream);

  // read chunk name
  Stream.Read(FChunkName, 4);

  // read yet unknown value
  Stream.Read(FUnknown, 4);

  with Stream do
  begin
    ChunkEnd := Position + FChunkSize - 12;
    Assert(ChunkEnd <= Stream.Size);
    while Position < ChunkEnd do
    begin
      Position := Position + 4;
      Read(SubChunkName, 4);
      Position := Position - 8;
      ConvertStreamToChunk(GetChunkClass(SubChunkName), Stream);
    end;
    if Position <> ChunkEnd then
      Position := ChunkEnd;

    // eventually skip padded zeroes
    if cfPadSize in ChunkFlags then
      Position := Position + CalculateZeroPad;
  end;

  // eventually skip padded zeroes
  if cfPadSize in ChunkFlags then
    Stream.Position := Stream.Position + CalculateZeroPad;
end;

procedure TMp4MetaChunk.SaveToStream(Stream: TStream);
var
  Value: Cardinal;
  i: Integer;
begin
  // write chunk size
  FChunkSize := GetChunkSize;
  WriteSwappedCardinal(Stream, FChunkSize);

  // write chunk name
  Stream.Write(FChunkName, 4);

  // write unknown
  Stream.Write(FUnknown, 4);

  for i := 0 to FChunkList.Count - 1 do
    FChunkList[i].SaveToStream(Stream);

  // insert pad byte if necessary
  if cfPadSize in ChunkFlags then
    Stream.Write(CZeroPad, CalculateZeroPad);
end;


{ TMp4MoovChunk }

constructor TMp4MoovChunk.Create;
begin
  inherited;

  RegisterChunkClasses([TMp4MovieHeaderChunk,
    TMp4InitialObjectDescriptorChunk, TMp4TrackChunk, TMp4UserDataChunk]);
end;

class function TMp4MoovChunk.GetClassChunkName: TChunkName;
begin
  Result := 'moov';
end;


{ TFileMp4 }

constructor TFileMp4.Create;
begin
  inherited;

  FChunkFlags := [cfSizeFirst, cfReversedByteOrder];
  FFileType := TMp4FileTypeChunk.Create;
end;

class function TFileMp4.CanLoad(Stream: TStream): Boolean;
var
  ChunkSize: Cardinal;
  ChunkName: TChunkName;
begin
  Result := Stream.Size >= 8;

  if Result then
  begin
    ChunkSize := ReadSwappedCardinal(Stream);
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
begin
  FFileType.LoadFromStream(Stream);
  AddChunk(FFileType);

  while Stream.Position + 8 <= Stream.Size do
  begin
    ChunkSize := ReadSwappedCardinal(Stream);
    Stream.Read(ChunkName, 4);
    Stream.Position := Stream.Position - 8;
    ReadUnknownChunk(Stream, ChunkName, ChunkSize);
  end;
end;

procedure TFileMp4.SaveToStream(Stream: TStream);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    SubChunk[Index].SaveToStream(Stream);
end;


initialization

  TFileMp4.RegisterChunks([TMp4FileTypeChunk, TMp4FreeChunk, TMp4MovieDataChunk,
    TMp4MoovChunk, TMp4MovieHeaderChunk, TMp4TrackChunk]);

end.
