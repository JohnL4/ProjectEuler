package com.how_hard_can_it_be.utils;

/**
 * Logging functions.  Currently logs to System.err.
 * @author John Lusk
 *
 */
public class Log
{
    public static void note( String aMsg)
    {
        String codeLocn = getCallerCodeLocation();
        System.err.printf( "NOTE: %s at %s\n", aMsg, codeLocn);
    }
    
    public static void warn( String aMsg)
    {
        String codeLocn = getCallerCodeLocation();
        System.err.printf( "WARNING: %s at %s\n", aMsg, codeLocn);
    }
    
    public static void error( String aMsg)
    {
        String codeLocn = getCallerCodeLocation();
        System.err.printf( "ERROR: %s at %s\n", aMsg, codeLocn);        
    }

    /**
     * Returns caller's (two stackframes previous) location in code.
     * @return
     */
    private static String getCallerCodeLocation()
    {
        StringBuffer retval = new StringBuffer();
        StackTraceElement stackTraceElt = Thread.currentThread().getStackTrace()[3];
        retval.append( String.format( "%s.%s", stackTraceElt.getClassName(),
                stackTraceElt.getMethodName()));
        if (stackTraceElt.getFileName() != null && stackTraceElt.getLineNumber() != -1)
            retval.append( String.format( " (%s:%d)", stackTraceElt.getFileName(), stackTraceElt.getLineNumber()));
        return retval.toString();
    }

}
