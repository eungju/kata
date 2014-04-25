#!python 
# rerun.py by June Kim <juneaftn at hanmail dot net>
#    idea taken from ward's build script at http://c2.com/~ward/io/IoGame/rerun
#
# usage: rerun.py <monitored filename> [optional runner with options]
# e.g.: rerun.py spiral.io io.exe

import sys,time,os,subprocess,shutil 
from difflib import unified_diff as diff #or context_diff

DEFAULT_RUNNER=''
delay=1.0 
 
if sys.platform=='win32': 
    def kill(pid):  #os.system('taskkill /F -pid %d'%pid) 
        try: 
            import win32api 
            PROCESS_TERMINATE = 1 
            handle = win32api.OpenProcess(PROCESS_TERMINATE, False, pid) 
            win32api.TerminateProcess(handle, -1) 
            win32api.CloseHandle(handle) 
            return True 
        except: 
            return False 
else: 
    def kill(pid): 
        os.system('kill %d'%pid) 

def timeDiff(seconds):
    mins,secs=divmod(seconds,60)
    return "%dm %ds"%(mins,secs)

def changecallback(fname,runner): 
    try: 
        pid=int(open('.lastRun','rt').read()) 
    except: 
        pass 
    else: 
        kill(pid) 
    print '_'*79
    try:
        past=open(fname+'~','r').read()
    except:
        print "[+] No previous file"
    else:
        print "[+] You changed..." 
        new=open(fname,'r').read()
        lastmtime=mtimeFor(fname+'~')
        currentmtime=mtimeFor(fname)
        print ''.join(diff(past.splitlines(1),new.splitlines(1),
                    'previous','current',
                    time.ctime(lastmtime),time.ctime(currentmtime))) 
        print "[+] %s elapsed since last run"%(timeDiff(currentmtime-lastmtime))
    p=subprocess.Popen(runner+' '+fname) 
    print "[+] %d started"%p.pid

    shutil.copyfile(fname,fname+'~') 
    open('.lastRun','w').write(str(p.pid)) 
 
def mtimeFor(fname): 
    return os.stat(fname).st_mtime 
 
def rerun(fname,onchange,runner): 
    last_mtime=mtimeFor(fname) 
    while True: 
        time.sleep(delay) 
        mtime=mtimeFor(fname) 
        if last_mtime!=mtime: 
            last_mtime=mtime 
            onchange(fname,runner) 
 
if __name__=='__main__': 
    fname=sys.argv[1] 
    if len(sys.argv)>2: 
        runner=' '.join(sys.argv[2:]) 
    else: 
        runner=DEFAULT_RUNNER 
    rerun(fname,onchange=changecallback,runner=runner) 
