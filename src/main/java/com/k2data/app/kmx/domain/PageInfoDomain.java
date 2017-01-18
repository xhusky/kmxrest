package com.k2data.app.kmx.domain;

/**
 * @author lidong 17-1-17.
 */
public class PageInfoDomain {

    private Integer pageNum;
    private Integer pageSize;
    private Integer size;

    public Integer getPageNum() {
        return pageNum;
    }

    public void setPageNum(Integer pageNum) {
        this.pageNum = pageNum;
    }

    public Integer getPageSize() {
        return pageSize;
    }

    public void setPageSize(Integer pageSize) {
        this.pageSize = pageSize;
    }

    public Integer getSize() {
        return size;
    }

    public void setSize(Integer size) {
        this.size = size;
    }

}
