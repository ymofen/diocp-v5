unit utils_locker;

interface


type
  PSpinLockerContext = ^TSpinLockerContext;
  TSpinLockerContext = record
    lock_flag:Integer;
  end;


procedure SpinLocker(var Target:Integer);

implementation

end.
