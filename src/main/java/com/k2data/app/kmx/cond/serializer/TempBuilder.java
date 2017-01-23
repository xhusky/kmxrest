package com.k2data.app.kmx.cond.serializer;

/**
 * @author lidong 17-1-22.
 */
public class TempBuilder {

    public void build() {
        String fieldGroup = "test1Group";

        nolist(
            field("fieldGroup", fieldGroup),
            field("fields", list("a", "b"))
        );
    }

    private TempObj field(String key, Object value) {
        TempObj obj = new TempObj();
        obj.setKey(key);
        obj.setValue(value);

        return obj;
    }

    private TempObj nolist(TempObj... tempObjs) {

        return null;
    }

    private TempObj list(Object... objects) {
        TempObj tempObj = new TempObj();
        tempObj.setKey("");
        tempObj.setValue(objects);

        return tempObj;
    }

}
