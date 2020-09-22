import {createLocalVue, shallowMount} from '@vue/test-utils'
import EditProfile from "../../../src/components/EditProfile/EditProfile";
import VueRouter from "vue-router";
import api from "../../../src/Api"
import Vuex from "vuex";

let wrapper;
jest.mock("../../../src/Api");

const propsData = {
    "id": 90032,
    "firstname":"Timmy",
    "lastname":"Tester",
    "middlename":"",
    "nickname":"",
    "primary_email":"tte174@uclive.ac.nz",
    "additional_email":[],
    "bio":"",
    "date_of_birth":"1971-08-02",
    "gender":"female",
    "fitness":5,
    "passports":[],
    "activities":["Cycling","E-sports"],
    "authLevel":5,
    "location":{
    "longitude":173,
        "latitude":-44,
        "address":"Christchurch,  Canterbury,  New Zealand,  8041"
    }
};

describe('EditProfile.vue', () => {
    api.getProfile.mockResolvedValue({data: propsData});
    let getters;
    let store;

    beforeEach(() => {
        getters = {
            getUserId: () => propsData.id,
            getAuthenticationLevel: () => 2
        };

        store = new Vuex.Store({
            getters
        });

        const localVue = createLocalVue();
        const router = new VueRouter();
        localVue.use(VueRouter);
        localVue.use(Vuex);
        router.push({
            params: {
                id: 90032
            }
        });
        wrapper = shallowMount(EditProfile, {
            localVue,
            router,
            store
        });
        wrapper.vm.warningToast = jest.fn();
    });

    afterEach(() => {
        wrapper.destroy();
    });

    it('Displays all tabs when logged in as a normal user', () => {
        const listItems = wrapper.findAll('li');
        expect(listItems.length).toEqual(7);
        for (let i = 0; i < listItems.length; i++) {
            expect(listItems.wrappers[i].isVisible()).toBe(true)
        }
    });

    it('Renders the basic info component upon starting', () => {
        const components = wrapper.vm.componentMap;
        let expectedIndex = 0;
        let component = components[expectedIndex];
        expect(wrapper.findComponent(component).isVisible());
    });
});
