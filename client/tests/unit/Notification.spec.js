import {createLocalVue, shallowMount} from '@vue/test-utils'
import Notification from "../../src/components/Notification";
import dateTimeMixin from "../../src/mixins/dateTimeMixin";
import Vuex from 'vuex';

const localVue = createLocalVue();
localVue.use(Vuex);

const dataTemplate = {
    "message": "John Smith started following the activity 'Fun Run.'",
    "notificationType": 3,
    "activityId": 20,
    "profileId": 0,
    "dateTime": "2020-09-05T08:00:00+1300"
};

describe('Notification.vue', () => {
    const userId = 10;
    let store;
    let getters;

    beforeEach(() => {
        getters = {
            getUserId: () => userId
        };

        store = new Vuex.Store({
            getters
        });
    });

    it('Displays the message correctly with normal data', () => {
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: dataTemplate}
        });
        const message = wrapper.find("#message");
        expect(message.text()).toBe(dataTemplate.message);
    });

    it('Displays the date correctly with normal data', () => {
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: dataTemplate}
        });
        const date = wrapper.find("#date");
        expect(date.text()).toBe(dateTimeMixin.methods.dateFormat(dataTemplate.dateTime));
    });

    it('Loads a self-triggered notification with the right colour', () => {
        let data = {};
        Object.assign(data, dataTemplate);

        data.profileId = userId;
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.vm.$data.cardStyle.backgroundColor).toBe("#ffffb0");
    });

    it('Loads an activity change notification with the right colour', () => {
        let data = {};
        Object.assign(data, dataTemplate);

        data.notificationType = 0;
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.vm.$data.cardStyle.backgroundColor).toBe("#a6b5ff");
    });

    it('Loads an member change notification with the right colour', () => {
        let data = {};
        Object.assign(data, dataTemplate);

        data.notificationType = 3;
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.vm.$data.cardStyle.backgroundColor).toBe("#99ff94");
    });
    //Can't figure out how to check routing rn
    /*
    it('Routes to the activity when selecting the "View Activity" button', () => {
        const $router = {
            push: jest.fn()
        };
        const wrapper = shallowMount(Notification, {
            store, localVue,
            propsData: {notification: dataTemplate},
            mocks: {
                $router
            }
        });

        const button = wrapper.find("#viewButton");
        button.trigger("click");

        expect($router.push).toHaveBeenCalled();
    });
     */
});