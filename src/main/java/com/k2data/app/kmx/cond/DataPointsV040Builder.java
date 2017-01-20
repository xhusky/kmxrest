package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.DataPointsDomain;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.Shift;
import com.k2data.app.kmx.enums.Sign;
import com.k2data.app.kmx.utils.Assert;

import java.util.*;

/**
 * data-points v0.4.0 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-16.
 */
public class DataPointsV040Builder extends KmxCondBuilder {

    private KmxInitParams initParams;

    private String fieldGroup;
    private String sampleTime;
    private Set<String> fields = new HashSet<>();

    private String idValue;
    private List<String> orIdValue = new ArrayList<>();
    private List<String> andIdValue = new ArrayList<>();

    private List<String> shift = new ArrayList<>();

    private String resultFormatIso;

    public DataPointsV040Builder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    /**
     * 生成查询条件
     *
     * @return 查询条件 json
     */
    @Override
    public KmxCond build() {
        Assert.notEmpty(fields, "Fields must not be null");
        Assert.notNull(sampleTime, "SampleTime must not be null");

//        noSignList(
//                "{",
//                "F", "fieldGroup", fieldGroup,
//                "L", "fields", new ArrayList<>(fields),
//                "F", "sampleTime", sampleTime,
//                "O", "coValueFilter",
//        );

        String aadd = aa(
                field("L", "fields", new ArrayList<>(fields)),
                field("F", "sampleTime", sampleTime),
                field("O", "coValueFilter",
                        field("O", "idFieldFilter", field("O", initParams.getIdField(), objField(Sign.EQ.getValue(), idValue)))
                ),
                field("O", "options",
                        field("F", "resultTimeFormat", resultFormatIso))
        );

//        String paramsSb = noSignList(
//                "{",
//                objField("fieldGroup", fieldGroup),
//                list("fields", new ArrayList<>(fields), true),
//                objField("sampleTime", sampleTime),
//                obj("coValueFilter",
//                        obj("idFieldFilter",
//                                obj(initParams.getIdField(), objField(Sign.EQ.getValue(), idValue)),
//                                list(Sign.OR.getValue(), orIdValue),
//                                list(Sign.AND.getValue(), andIdValue)
//                        )
//                ),
//                list("shift", shift),
//                obj("options", objField("resultTimeFormat", resultFormatIso)),
//                "}"
//        );

        Map<String, String> params = new HashMap<>();
        params.put("query", aadd);

        KmxCond cond = new KmxCond();
        cond.setUrl(initParams.getUrls().get(KmxCondType.dataPoints));
        cond.setParams(params);
        cond.setClazz(DataPointsDomain.class);

        return cond;
    }

    public DataPointsV040Builder fieldGroup(String fieldGroup) {
        this.fieldGroup = fieldGroup;
        return this;
    }

    public DataPointsV040Builder sampleTime(Date sampleTime) {
        this.sampleTime = sampleTime.toInstant().toString().replace("Z", "%2B08:00");
        return this;
    }

    public DataPointsV040Builder sampleTime(String sampleTime) {
        this.sampleTime = sampleTime;
        return this;
    }

    /* fields begin */
    public DataPointsV040Builder fields(Set<String> fields) {
        this.fields = fields;
        return this;
    }

    public DataPointsV040Builder fields(List<String> fields) {
        this.fields = new HashSet<>(fields);
        return this;
    }

    public DataPointsV040Builder fields(String[] fields) {
        Collections.addAll(this.fields, fields);
        return this;
    }

    public DataPointsV040Builder field(String field) {
        this.fields.add(field);
        return this;
    }
    /* fields end */

    /* shift begin */
    public DataPointsV040Builder shift(Shift shift) {
        this.shift.add(buildShift("$default", shift));
        return this;
    }

    public DataPointsV040Builder shift(String field, Shift shift) {
        this.shift.add(buildShift(field, shift));
        return this;
    }

    private String buildShift(String field, Shift shift) {
        return String.format("{ \"%s\": \"%s\" }", field, shift.getLowerValue());
    }
    /* shift end */

    public DataPointsV040Builder idValue(String idValue) {
        this.idValue = idValue;
        return this;
    }

    public DataPointsV040Builder orIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataPointsV040Builder andIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataPointsV040Builder resultFormatIso() {
        this.resultFormatIso = "iso";
        return this;
    }

}
