package com.k2data.app.kmx.cond.serializer;

import com.k2data.app.kmx.cond.KmxCondBuilder;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * @author lidong 17-1-20.
 */
public class TempObj {

    private SerializeWriter writer;

    private String key;
    private Object[] value;
    private String type;



    public void write() {
        try {
            switch (type) {
                case "F":
                    KmxCondBuilder.serializerMap.get("F").write(writer, key, value, "", null);
                case "O":
                    KmxCondBuilder.serializerMap.get("O").write(writer, key, value, "", null);
                case "L":
                    KmxCondBuilder.serializerMap.get("L").write(writer, key, value, "", null);
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }

    }

    public SerializeWriter getWriter() {
        return writer;
    }

    public void setWriter(SerializeWriter writer) {
        this.writer = writer;
    }

    public String getKey() {
        return key;
    }

    public void setKey(String key) {
        this.key = key;
    }

    public Object[] getValue() {
        return value;
    }

    public void setValue(Object[] value) {
        this.value = value;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }
}
