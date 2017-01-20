package com.k2data.app.kmx.cond.serializer;

import java.io.IOException;
import java.lang.reflect.Type;

/**
 * @author lidong 17-1-20.
 */
public class FieldSerializer implements BaseSerializer {

    @Override
    public void write(SerializeWriter writer, String key, Object object, Object fieldName, Type fieldType) throws IOException {
        writer.append("{ \"")
                .append(key)
                .append("\": ")
                .append(((Object[])object)[0].toString())
                .append(" }");
    }

}
