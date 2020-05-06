import { shallowMount } from '@vue/test-utils'
import AddActivity from "../../src/components/AddActivity";

let wrapper;

beforeEach(() => {
    wrapper = shallowMount(AddActivity, {
        propsData: {},
        mocks: {},
        stubs: {},
        methods: {},
    });
    wrapper.vm.showWarning = jest.fn();
});

afterEach(() => {
    wrapper.destroy();
});

describe('AddActivity.vue', () => {
    test('Activity is invalid without types', () => {
        wrapper.vm.chosenActivityTypes = []
        expect(wrapper.vm.validateActivity()).toBeFalsy();
    });
    test('Valid continous activity passes checks', () =>{
        wrapper.vm.chosenActivityTypes = ["Hiking"]
        wrapper.vm.isContinous = true
        expect(wrapper.vm.validateActivity()).toBeTruthy();
    })
    test('Adding activity type with no name', () => {
        wrapper.vm.chosenActivityTypes = []
        wrapper.vm.newActivityType = ""
        wrapper.vm.addActivityType()
        expect(wrapper.vm.chosenActivityTypes).toStrictEqual([]);
    });
    test('Adding activity type already chosen', () => {
        wrapper.vm.chosenActivityTypes = ["Running", "Cycling"]
        wrapper.vm.newActivityType = "Running"
        wrapper.vm.addActivityType()
        expect(wrapper.vm.chosenActivityTypes).toStrictEqual(["Running", "Cycling"]);
    });
    test('Deleting activity type', () => {
        wrapper.vm.chosenActivityTypes = ["Running", "Hiking"]
        wrapper.vm.deleteActivityType("Running")
        expect(wrapper.vm.chosenActivityTypes).toStrictEqual(["Hiking"]);
    });
});
