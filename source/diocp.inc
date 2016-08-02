{$DEFINE DIOCP}

{$IFDEF DEBUG}
  {$DEFINE DEBUG_ON}
{$ENDIF}

{.$DEFINE SOCKET_REUSE}

{$DEFINE USE_HASHTABLE}

// http://delphi.wikia.com/wiki/Borland_Compiler_Conditional_Defines
// CodeGear Delphi 2007 for Win32	11.0	VER180 and VER185	  18, 18.5
{$IF defined(FPC) or (RTLVersion>=18))}
  {$DEFINE HAVE_INLINE}
{$ELSE}

{$IFEND RTLVersion}

{$if CompilerVersion < 15}  // <D7
{.$DEFINE D6} 
{$ifend}

