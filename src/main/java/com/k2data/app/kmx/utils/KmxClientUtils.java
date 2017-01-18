package com.k2data.app.kmx.utils;

import com.k2data.app.kmx.KmxException;

import java.text.ParsePosition;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * @author lidong 17-1-16.
 */
public class KmxClientUtils {

    public static final String ISO_DATE_PATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'";

    /**
     * <p>Checks if a CharSequence is whitespace, empty ("") or null.</p>
     *
     * <pre>
     * StringUtils.isBlank(null)      = true
     * StringUtils.isBlank("")        = true
     * StringUtils.isBlank(" ")       = true
     * StringUtils.isBlank("bob")     = false
     * StringUtils.isBlank("  bob  ") = false
     * </pre>
     *
     * @param cs  the CharSequence to check, may be null
     * @return {@code true} if the CharSequence is null, empty or whitespace
     * @since 2.0
     * @since 3.0 Changed signature from isBlank(String) to isBlank(CharSequence)
     */
    public static boolean isBlank(final CharSequence cs) {
        int strLen;
        if (cs == null || (strLen = cs.length()) == 0) {
            return true;
        }
        for (int i = 0; i < strLen; i++) {
            if (!Character.isWhitespace(cs.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    public static boolean isEmpty(List<String> list) {
        return list == null || list.size() == 0;
    }

    public static boolean isEmpty(Object[] array) {
        return (array == null || array.length == 0);
    }

    public static boolean isEmpty(Collection<?> collection) {
        return (collection == null || collection.isEmpty());
    }

    public static boolean isEmpty(Map<?, ?> map) {
        return (map == null || map.isEmpty());
    }

    public static boolean hasLength(CharSequence str) {
        return (str != null && str.length() > 0);
    }

    public static Date formatDate(String str, String pattern) {
        if (str == null || pattern == null) {
            throw new IllegalArgumentException("Date and Patterns must not be null");
        }

        SimpleDateFormat parser = new SimpleDateFormat();

        parser.setLenient(false);
        final ParsePosition pos = new ParsePosition(0);

        // LANG-530 - need to make sure 'ZZ' output doesn't get passed to SimpleDateFormat
        if (pattern.endsWith("ZZ")) {
            pattern = pattern.substring(0, pattern.length() - 1);
        }

        parser.applyPattern(pattern);
        pos.setIndex(0);

        String str2 = str;
        // LANG-530 - need to make sure 'ZZ' output doesn't hit SimpleDateFormat as it will ParseException
        if (pattern.endsWith("ZZ")) {
            str2 = str.replaceAll("([-+][0-9][0-9]):([0-9][0-9])$", "$1$2");
        }

        final Date date = parser.parse(str2, pos);
        if (date != null && pos.getIndex() == str2.length()) {
            return date;
        }

        throw new KmxException("Unable to parse the date: " + str);
    }

}
