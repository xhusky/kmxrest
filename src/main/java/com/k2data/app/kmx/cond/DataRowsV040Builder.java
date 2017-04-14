package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.DataRowsDomain;
import com.k2data.app.kmx.enums.Aggregation;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.Sign;
import com.k2data.app.kmx.utils.Assert;

import java.util.*;

/**
 * data-rows v0.4.0 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-16.
 */
public class DataRowsV040Builder extends KmxCondBuilder {

    private KmxInitParams initParams;

    private String fieldGroup;
    private String start;
    private String end;
    private Set<String> fields = new HashSet<>();

    private List<String> valueFilters = new ArrayList<>();
    private List<String> valueTrans = new ArrayList<>();

    private String idValue;
    private List<String> orIdValue = new ArrayList<>();
    private List<String> andIdValue = new ArrayList<>();
    private List<String> orNonIdFieldFilter = new ArrayList<>();
    private List<String> andNonIdFieldFilter = new ArrayList<>();

    private List<String> aggregations = new ArrayList<>();
    private String interval;
    private Boolean naturalTimeBoundary;
    private Object fill;

    private String order = "asc";   // 排序
    private Integer size;    // 每页大小
    private Integer page;    // 第几页

    private String resultFormatIso;

    public DataRowsV040Builder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    /**
     * 生成查询条件
     *
     * @return 查询条件 json
     */
    @Override
    public KmxCond build() {
        Assert.notEmpty(fields, "Field must not be null");
        Assert.notNull(start, "Start must not be null");
        Assert.notNull(end, "End must not be null");

        String paramsSb = "{" + noSignList(
                objField("fieldGroup", fieldGroup),
                list("fields", new ArrayList<>(fields), true),
                obj("timeRange",
                        objField("start", start),
                        objField("end", end)
                ),
                list("valueFilters", valueFilters),
                list("valueTrans", valueTrans),
                obj("coValueFilter",
                        obj("idFieldFilter",
                                obj(initParams.getIdField(), objField(Sign.EQ.getValue(), idValue)),
                                list(Sign.OR.getValue(), orIdValue),
                                list(Sign.AND.getValue(), andIdValue)
                        ),
                        obj("nonIdFieldFilter",
                                list(Sign.AND.getValue(),
                                        andNonIdFieldFilter
                                ),
                                list(Sign.OR.getValue(),
                                        orNonIdFieldFilter
                                )
                        )
                ),
                list("aggregations", aggregations),
                obj("aggregationOptions",
                        objField("interval", interval),
                        objField("naturalTimeBoundary", naturalTimeBoundary),
                        objField("fill", fill)
                ),
                objField("order", order),
                objField("size", size),
                objField("page", page),
                obj("options", objField("resultTimeFormat", resultFormatIso))
        ) + "}";

        Map<String, String> params = new HashMap<>();
        params.put("query", paramsSb);

        KmxCond cond = new KmxCond();
        cond.setUrl(initParams.getUrls().get(KmxCondType.dataRows));
        cond.setParams(params);
        cond.setClazz(DataRowsDomain.class);

        return cond;
    }

    public DataRowsV040Builder fieldGroup(String fieldGroup) {
        this.fieldGroup = fieldGroup;
        return this;
    }

    public DataRowsV040Builder start(Date start) {
        this.start = start.toInstant().toString().replace("Z", "%2B08:00");
        return this;
    }

    public DataRowsV040Builder start(String start) {
        this.start = start;
        return this;
    }

    public DataRowsV040Builder end(Date end) {
        this.end = end.toInstant().toString().replace("Z", "%2B08:00");
        return this;
    }

    public DataRowsV040Builder end(String end) {
        this.end = end;
        return this;
    }

    /* fields begin */
    public DataRowsV040Builder fields(Set<String> fields) {
        this.fields = fields;
        return this;
    }

    public DataRowsV040Builder fields(List<String> fields) {
        this.fields = new HashSet<>(fields);
        return this;
    }

    public DataRowsV040Builder fields(String[] fields) {
        Collections.addAll(this.fields, fields);
        return this;
    }

    public DataRowsV040Builder field(String field) {
        this.fields.add(field);
        return this;
    }
    /* fields end */

    public DataRowsV040Builder idValue(String idValue) {
        this.idValue = idValue;
        return this;
    }

    public DataRowsV040Builder orIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataRowsV040Builder andIdValue(String field, String value) {
        this.andIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataRowsV040Builder andNonIdFieldFilter(String field, Sign fieldSign, Object value) {
        Object val = value instanceof String ? "\"" + value + "\"" : value;
        this.andNonIdFieldFilter.add(String.format("{ \"%s\": { \"%s\": %s } }", field, fieldSign.getValue(), val));
        return this;
    }

    public DataRowsV040Builder orNonIdFieldFilter(String field, Sign fieldSign, Object value) {
        Object val = value instanceof String ? "\"" + value + "\"" : value;
        this.orNonIdFieldFilter.add(String.format("{ \"%s\": { \"%s\": %s } }", field, fieldSign.getValue(), val));
        return this;
    }

    public DataRowsV040Builder valueFilters(List<String> valueFilters) {
        this.valueFilters = valueFilters;
        return this;
    }

    public DataRowsV040Builder valueFilters(String valueFilters) {
        this.valueFilters.add(valueFilters);
        return this;
    }

    public DataRowsV040Builder valueTrans(List<String> valueTrans) {
        this.valueTrans = valueTrans;
        return this;
    }

    public DataRowsV040Builder valueTrans(String key, String value) {
        this.valueTrans.add("{ \"" + key + "\": " + "\"" + value + "\" }");
        return this;
    }

    public DataRowsV040Builder valueTrans(String valueTrans) {
        this.valueTrans.add(valueTrans);
        return this;
    }

    public DataRowsV040Builder aggregations(String aggregations) {
        this.aggregations.add(aggregations);
        return this;
    }

    public DataRowsV040Builder aggregations(Aggregation... aggregations) {
        this.aggregations.add(buildAggregations("$default", aggregations));
        return this;
    }

    public DataRowsV040Builder aggregations(String field, Aggregation... aggregations) {
        this.aggregations.add(buildAggregations(field, aggregations));
        return this;
    }

    public DataRowsV040Builder aggregations(String field, String name, Aggregation aggregation) {
        this.aggregations.add(String.format("{ \"%s\": {\"type\": \"%s\",\"name\": \"%s\"} }",
                field,
                aggregation.getLowerValue(),
                name));
        return this;
    }

    private String buildAggregations(String field, Aggregation... aggregations) {
        StringBuilder sb = new StringBuilder();
        sb.append("{ \"").append(field).append("\": [");
        for (int i = 0; i < aggregations.length; i++) {
            Aggregation aggregation = aggregations[i];

            if (i != 0) {
                sb.append(",");
            }

            sb.append("\"").append(aggregation.getLowerValue()).append("\"");
        }
        sb.append("] }");

        return sb.toString();
    }

    public DataRowsV040Builder interval(String interval) {
        this.interval = interval;
        return this;
    }

    public DataRowsV040Builder naturalTimeBoundary(boolean naturalTimeBoundary) {
        this.naturalTimeBoundary = naturalTimeBoundary;
        return this;
    }

    public DataRowsV040Builder fill(Object fill) {
        this.fill = fill;
        return this;
    }

    public DataRowsV040Builder desc() {
        this.order = "desc";
        return this;
    }

    public DataRowsV040Builder size(Integer size) {
        this.size = size;
        return this;
    }

    public DataRowsV040Builder page(Integer page) {
        this.page = page;
        return this;
    }

    public DataRowsV040Builder resultFormatIso() {
        this.resultFormatIso = "iso";
        return this;
    }

}
