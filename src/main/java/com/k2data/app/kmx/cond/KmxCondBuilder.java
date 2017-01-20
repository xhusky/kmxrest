package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.cond.serializer.*;

import java.util.*;

/**
 * @author lidong 17-1-11.
 */
public abstract class KmxCondBuilder {

    public abstract KmxCond build();

    private SerializeWriter serializeWriter = new SerializeWriter();

    public static Map<String, BaseSerializer> serializerMap = new HashMap<>();

    public static BaseSerializer get(String key) {
        return serializerMap.get(key);
    }

    public KmxCondBuilder() {
        serializerMap.put("F", new FieldSerializer());
        serializerMap.put("O", new ObjSerializer());
        serializerMap.put("L", new ListSerializer());
    }

    public TempObj field(String type, String key, Object... value) {
        TempObj obj = new TempObj();
        obj.setType(type);
        obj.setKey(key);
        obj.setValue(value);

        obj.setWriter(this.serializeWriter);

        return obj;

//        switch (type) {
//            case "F":
//
//            case "O":
//
//            case "L":
//
//        }
    }

    public String aa(TempObj... tempObjs) {
        for (TempObj obj : tempObjs) {
            obj.write();
        }

        serializeWriter.flush();
        String out = serializeWriter.toString();
        serializeWriter.close();

        return out;
    }

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

    public String noSignList(SerializeWriter... serializeWriters) {

        this.serializeWriter.flush();
        String out = this.serializeWriter.toString();
        this.serializeWriter.close();

        return out;
    }

    public String noSignList(String... fields) {



        int i = 0;
        for (String field : fields) {
            if (field == null) {
                continue;
            }

            if (i++ != 0) {
                serializeWriter.write(",");
            }

            serializeWriter.write(field);
        }

        serializeWriter.flush();
        String out = serializeWriter.toString();
        serializeWriter.close();

        return out;


//        StringBuilder innerSb = new StringBuilder();
//
//        int i = 0;
//        for (String field : fields) {
//            if (field == null) {
//                continue;
//            }
//
//            if (i++ != 0) {
//                innerSb.append(",");
//            }
//
//            innerSb.append(field);
//        }
//
//        return innerSb.toString();
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
