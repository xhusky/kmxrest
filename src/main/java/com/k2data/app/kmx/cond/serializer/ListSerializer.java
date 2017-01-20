package com.k2data.app.kmx.cond.serializer;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.List;

/**
 * @author lidong 17-1-20.
 */
public class ListSerializer implements BaseSerializer {

    @Override
    public void write(SerializeWriter writer, String key, Object object, Object fieldName, Type fieldType) throws IOException {

        Object[] val = (Object[]) object;

        writer.append("\"").append(key).append("\": [");
        for (Object value : val) {
            writer.append(value.toString());
        }
        writer.append("]");
    }

}
