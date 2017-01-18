package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.DataPointsDomain;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.Shift;
import com.k2data.app.kmx.enums.Sign;
import com.k2data.app.kmx.utils.Assert;

import java.util.*;

/**
 * data-points v1.2 查询条件 builder
 *
 * @author lidong 17-1-16.
 */
public class DataPointsV120Builder extends KmxCondBuilder {

    private KmxInitParams initParams;

    private String fieldGroup;
    private String sampleTime;
    private Set<String> fields = new HashSet<>();

    private String idValue;
    private List<String> orIdValue = new ArrayList<>();
    private List<String> andIdValue = new ArrayList<>();

    private List<String> shift = new ArrayList<>();

    private String resultFormatIso;

    public DataPointsV120Builder(KmxInitParams initParams) {
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

        String paramsSb = "{" + noSignList(
                objField("fieldGroup", fieldGroup),
                list("fields", new ArrayList<>(fields), true),
                objField("sampleTime", sampleTime),
                obj("coValueFilter",
                        obj("idFieldFilter",
                                obj(initParams.getIdField(), objField(Sign.EQ.getValue(), idValue)),
                                list("$or", orIdValue),
                                list("$and", andIdValue)
                        )
                ),
                list("shift", shift),
                obj("options", objField("resultTimeFormat", resultFormatIso))
        ) + "}";

        Map<String, String> params = new HashMap<>();
        params.put("query", paramsSb);

        KmxCond cond = new KmxCond();
        cond.setUrl(initParams.getUrls().get(KmxCondType.dataPoints));
        cond.setParams(params);
        cond.setClazz(DataPointsDomain.class);

        return cond;
    }

    public DataPointsV120Builder fieldGroup(String fieldGroup) {
        this.fieldGroup = fieldGroup;
        return this;
    }

    public DataPointsV120Builder sampleTime(Date sampleTime) {
        this.sampleTime = sampleTime.toInstant().toString().replace("Z", "%2B08:00");
        return this;
    }

    public DataPointsV120Builder sampleTime(String sampleTime) {
        this.sampleTime = sampleTime;
        return this;
    }

    /* fields begin */
    public DataPointsV120Builder fields(Set<String> fields) {
        this.fields = fields;
        return this;
    }

    public DataPointsV120Builder fields(List<String> fields) {
        this.fields = new HashSet<>(fields);
        return this;
    }

    public DataPointsV120Builder fields(String[] fields) {
        Collections.addAll(this.fields, fields);
        return this;
    }

    public DataPointsV120Builder field(String field) {
        this.fields.add(field);
        return this;
    }
    /* fields end */

    /* shift begin */
    public DataPointsV120Builder shift(Shift shift) {
        this.shift.add(buildShift("$default", shift));
        return this;
    }

    public DataPointsV120Builder shift(String field, Shift shift) {
        this.shift.add(buildShift(field, shift));
        return this;
    }

    private String buildShift(String field, Shift shift) {
        return String.format("{ \"%s\": \"%s\" }", field, shift.getLowerValue());
    }
    /* shift end */

    public DataPointsV120Builder idValue(String idValue) {
        this.idValue = idValue;
        return this;
    }

    public DataPointsV120Builder orIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataPointsV120Builder andIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataPointsV120Builder resultFormatIso() {
        this.resultFormatIso = "iso";
        return this;
    }

}
