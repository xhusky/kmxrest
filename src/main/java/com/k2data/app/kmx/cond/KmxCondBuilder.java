package com.k2data.app.kmx.cond;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author lidong 17-1-11.
 */
public abstract class KmxCondBuilder {

    public abstract KmxCond build();

    public String objField(String key, Object value) {
        return objField(key, value, true);
    }

    public String objField(String key, Object value, boolean checkSpecialChar) {
        if (value == null) {
            return null;
        }

        if (checkSpecialChar) {
            value = value instanceof String ? "\"" + value + "\"" : value;
        }

        return String.format("\"%s\": %s", key, value.toString());
    }

    public String obj(String key, String... fields) {
        if (fields.length == 0) {
            return null;
        }

        StringBuilder innerSb = new StringBuilder()
                .append("\"").append(key).append("\": ").append("{");

        int i = 0;
        for (String field : fields) {
            if (field == null) {
                continue;
            }

            if (i++ != 0) {
                innerSb.append(",");
            }

            innerSb.append(field);
        }

        if (i == 0) {
            return null;
        }

        innerSb.append("}");

        return innerSb.toString();
    }

    public String noSignList(String... fields) {
        StringBuilder innerSb = new StringBuilder();

        int i = 0;
        for (String field : fields) {
            if (field == null) {
                continue;
            }

            if (i++ != 0) {
                innerSb.append(",");
            }

            innerSb.append(field);
        }

        return innerSb.toString();
    }

    public String list(String key, String... fields) {
        List<String> innerList = new ArrayList<>();
        Collections.addAll(innerList, fields);

        return list(key, innerList);
    }

    public String list(String key, List<String> fields) {
        return list(key, fields, false);
    }

    public String list(String key, List<String> fields, boolean checkSpecialChar) {
        if (fields == null || fields.size() == 0) {
            return null;
        }

        StringBuilder innerSb = new StringBuilder()
                .append("\"").append(key).append("\": ").append("[");

        int i = 0;
        for (String field : fields) {
            if (i++ != 0) {
                innerSb.append(",");
            }

            innerSb.append(checkSpecialChar ? "\"" + field + "\"" : field);
        }

        innerSb.append("]");

        return innerSb.toString();
    }

}
