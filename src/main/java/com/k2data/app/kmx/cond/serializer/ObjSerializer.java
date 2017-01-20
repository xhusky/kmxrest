package com.k2data.app.kmx.cond.serializer;

import java.io.IOException;
import java.lang.reflect.Type;
import java.util.List;
import java.util.Objects;

/**
 * @author lidong 17-1-20.
 */
public class ObjSerializer implements BaseSerializer {

    @Override
    public void write(SerializeWriter writer, String key, Object object, Object fieldName, Type fieldType) throws IOException {
        writer.append('\"')
                .append(key)
                .append("\": { ");

//        List<?> valList = (List<?>) object;

        Object[] val = (Object[]) object;

        int i = 0;
        for (Object item : val) {
            if (i++ != 0) {
                writer.append(',');
            }

            if (item.getClass().isAssignableFrom(SerializeWriter.class)) {

            } else {
                writer.append(Objects.toString(item));
            }
        }

        writer.append(" }");
    }

}
